/* A compiler for the "bf" language.  */

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "libgccjit.h"

/* Make "main" function:
     int
     main (int argc, char **argv)
     {
       ...
     }
*/
static gcc_jit_function *
make_main (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *param_argc =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "argc");
  gcc_jit_type *char_ptr_ptr_type =
    gcc_jit_type_get_pointer (
      gcc_jit_type_get_pointer (
	gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR)));
  gcc_jit_param *param_argv =
    gcc_jit_context_new_param (ctxt, NULL, char_ptr_ptr_type, "argv");
  gcc_jit_param *params[2] = {param_argc, param_argv};
  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  2, params,
				  0);
  return func_main;
}

#define MAX_OPEN_PARENS 16

typedef struct bf_compiler
{
  const char *filename;
  int line;
  int column;

  gcc_jit_context *ctxt;

  gcc_jit_type *void_type;
  gcc_jit_type *int_type;
  gcc_jit_type *byte_type;
  gcc_jit_type *array_type;

  gcc_jit_function *func_getchar;
  gcc_jit_function *func_putchar;

  gcc_jit_function *func;
  gcc_jit_block *curblock;

  gcc_jit_rvalue *int_zero;
  gcc_jit_rvalue *int_one;
  gcc_jit_rvalue *byte_zero;
  gcc_jit_rvalue *byte_one;
  gcc_jit_lvalue *data_cells;
  gcc_jit_lvalue *idx;

  int num_open_parens;
  gcc_jit_block *paren_test[MAX_OPEN_PARENS];
  gcc_jit_block *paren_body[MAX_OPEN_PARENS];
  gcc_jit_block *paren_after[MAX_OPEN_PARENS];

} bf_compiler;

/* Bail out, with a message on stderr.  */

static void
fatal_error (bf_compiler *bfc, const char *msg)
{
  fprintf (stderr,
	   "%s:%i:%i: %s",
	   bfc->filename, bfc->line, bfc->column, msg);
  abort ();
}

/* Get "data_cells[idx]" as an lvalue.  */

static gcc_jit_lvalue *
bf_get_current_data (bf_compiler *bfc, gcc_jit_location *loc)
{
  return gcc_jit_context_new_array_access (
    bfc->ctxt,
    loc,
    gcc_jit_lvalue_as_rvalue (bfc->data_cells),
    gcc_jit_lvalue_as_rvalue (bfc->idx));
}

/* Get "data_cells[idx] == 0" as a boolean rvalue.  */

static gcc_jit_rvalue *
bf_current_data_is_zero (bf_compiler *bfc, gcc_jit_location *loc)
{
  return gcc_jit_context_new_comparison (
    bfc->ctxt,
    loc,
    GCC_JIT_COMPARISON_EQ,
    gcc_jit_lvalue_as_rvalue (bf_get_current_data (bfc, loc)),
    bfc->byte_zero);
}

/* Compile one bf character.  */

static void
bf_compile_char (bf_compiler *bfc,
		 unsigned char ch)
{
  gcc_jit_location *loc =
    gcc_jit_context_new_location (bfc->ctxt,
				  bfc->filename,
				  bfc->line,
				  bfc->column);

  /* Turn this on to trace execution, by injecting putchar ()
     of each source char. */
  if (0)
    {
      gcc_jit_rvalue *arg =
	gcc_jit_context_new_rvalue_from_int (
					     bfc->ctxt,
					     bfc->int_type,
					     ch);
      gcc_jit_rvalue *call =
	gcc_jit_context_new_call (bfc->ctxt,
				  loc,
				  bfc->func_putchar,
				  1, &arg);
      gcc_jit_block_add_eval (bfc->curblock,
			      loc,
			      call);
    }

  switch (ch)
    {
      case '>':
	gcc_jit_block_add_comment (bfc->curblock,
				   loc,
				   "'>': idx += 1;");
	gcc_jit_block_add_assignment_op (bfc->curblock,
					 loc,
					 bfc->idx,
					 GCC_JIT_BINARY_OP_PLUS,
					 bfc->int_one);
	break;

      case '<':
	gcc_jit_block_add_comment (bfc->curblock,
				   loc,
				   "'<': idx -= 1;");
	gcc_jit_block_add_assignment_op (bfc->curblock,
					 loc,
					 bfc->idx,
					 GCC_JIT_BINARY_OP_MINUS,
					 bfc->int_one);
	break;

      case '+':
	gcc_jit_block_add_comment (bfc->curblock,
				   loc,
				   "'+': data[idx] += 1;");
	gcc_jit_block_add_assignment_op (bfc->curblock,
					 loc,
					 bf_get_current_data (bfc, loc),
					 GCC_JIT_BINARY_OP_PLUS,
					 bfc->byte_one);
	break;

      case '-':
	gcc_jit_block_add_comment (bfc->curblock,
				   loc,
				   "'-': data[idx] -= 1;");
	gcc_jit_block_add_assignment_op (bfc->curblock,
					 loc,
					 bf_get_current_data (bfc, loc),
					 GCC_JIT_BINARY_OP_MINUS,
					 bfc->byte_one);
	break;

      case '.':
	{
	  gcc_jit_rvalue *arg =
	    gcc_jit_context_new_cast (
	      bfc->ctxt,
	      loc,
	      gcc_jit_lvalue_as_rvalue (bf_get_current_data (bfc, loc)),
	      bfc->int_type);
	  gcc_jit_rvalue *call =
	    gcc_jit_context_new_call (bfc->ctxt,
				      loc,
				      bfc->func_putchar,
				      1, &arg);
	  gcc_jit_block_add_comment (bfc->curblock,
				     loc,
				     "'.': putchar ((int)data[idx]);");
	  gcc_jit_block_add_eval (bfc->curblock,
				  loc,
				  call);
	}
	break;

      case ',':
	{
	  gcc_jit_rvalue *call =
	    gcc_jit_context_new_call (bfc->ctxt,
				      loc,
				      bfc->func_getchar,
				      0, NULL);
	  gcc_jit_block_add_comment (
	    bfc->curblock,
	    loc,
	    "',': data[idx] = (unsigned char)getchar ();");
	  gcc_jit_block_add_assignment (bfc->curblock,
					loc,
					bf_get_current_data (bfc, loc),
					gcc_jit_context_new_cast (
					  bfc->ctxt,
					  loc,
					  call,
					  bfc->byte_type));
	}
	break;

      case '[':
	{
	  gcc_jit_block *loop_test =
	    gcc_jit_function_new_block (bfc->func, NULL);
	  gcc_jit_block *on_zero =
	    gcc_jit_function_new_block (bfc->func, NULL);
	  gcc_jit_block *on_non_zero =
	    gcc_jit_function_new_block (bfc->func, NULL);

	  if (bfc->num_open_parens == MAX_OPEN_PARENS)
	    fatal_error (bfc, "too many open parens");

	  gcc_jit_block_end_with_jump (
	    bfc->curblock,
	    loc,
	    loop_test);

	  gcc_jit_block_add_comment (
	    loop_test,
	    loc,
	    "'['");
	  gcc_jit_block_end_with_conditional (
	    loop_test,
	    loc,
	    bf_current_data_is_zero (bfc, loc),
	    on_zero,
	    on_non_zero);
	  bfc->paren_test[bfc->num_open_parens] = loop_test;
	  bfc->paren_body[bfc->num_open_parens] = on_non_zero;
	  bfc->paren_after[bfc->num_open_parens] = on_zero;
	  bfc->num_open_parens += 1;
	  bfc->curblock = on_non_zero;
	}
	break;

      case ']':
	{
	  gcc_jit_block_add_comment (
	    bfc->curblock,
	    loc,
	    "']'");

	  if (bfc->num_open_parens == 0)
	    fatal_error (bfc, "mismatching parens");
	  bfc->num_open_parens -= 1;
	  gcc_jit_block_end_with_jump (
	    bfc->curblock,
	    loc,
	    bfc->paren_test[bfc->num_open_parens]);
	  bfc->curblock = bfc->paren_after[bfc->num_open_parens];
	}
	break;

    case '\n':
      bfc->line +=1;
      bfc->column = 0;
      break;
    }

  if (ch != '\n')
    bfc->column += 1;
}

/* Compile the given .bf file into a gcc_jit_context, containing a
   single "main" function suitable for compiling into an executable.  */

gcc_jit_context *
bf_compile (const char *filename)
{
  bf_compiler bfc;
  FILE *f_in;
  int ch;

  memset (&bfc, 0, sizeof (bfc));

  bfc.filename = filename;
  f_in = fopen (filename, "r");
  if (!f_in)
    fatal_error (&bfc, "unable to open file");
  bfc.line = 1;

  bfc.ctxt = gcc_jit_context_acquire ();

  gcc_jit_context_set_int_option (
    bfc.ctxt,
    GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
    3);
  gcc_jit_context_set_bool_option (
    bfc.ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
    0);
  gcc_jit_context_set_bool_option (
    bfc.ctxt,
    GCC_JIT_BOOL_OPTION_DEBUGINFO,
    1);
  gcc_jit_context_set_bool_option (
    bfc.ctxt,
    GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
    0);
  gcc_jit_context_set_bool_option (
    bfc.ctxt,
    GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
    0);

  bfc.void_type =
    gcc_jit_context_get_type (bfc.ctxt, GCC_JIT_TYPE_VOID);
  bfc.int_type =
    gcc_jit_context_get_type (bfc.ctxt, GCC_JIT_TYPE_INT);
  bfc.byte_type =
    gcc_jit_context_get_type (bfc.ctxt, GCC_JIT_TYPE_UNSIGNED_CHAR);
  bfc.array_type =
    gcc_jit_context_new_array_type (bfc.ctxt,
				    NULL,
				    bfc.byte_type,
				    30000);

  bfc.func_getchar =
    gcc_jit_context_new_function (bfc.ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  bfc.int_type,
				  "getchar",
				  0, NULL,
				  0);

  gcc_jit_param *param_c =
    gcc_jit_context_new_param (bfc.ctxt, NULL, bfc.int_type, "c");
  bfc.func_putchar =
    gcc_jit_context_new_function (bfc.ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  bfc.void_type,
				  "putchar",
				  1, &param_c,
				  0);

  bfc.func = make_main (bfc.ctxt);
   bfc.curblock =
    gcc_jit_function_new_block (bfc.func, "initial");
  bfc.int_zero = gcc_jit_context_zero (bfc.ctxt, bfc.int_type);
  bfc.int_one = gcc_jit_context_one (bfc.ctxt, bfc.int_type);
  bfc.byte_zero = gcc_jit_context_zero (bfc.ctxt, bfc.byte_type);
  bfc.byte_one = gcc_jit_context_one (bfc.ctxt, bfc.byte_type);

  bfc.data_cells =
    gcc_jit_context_new_global (bfc.ctxt, NULL,
				 GCC_JIT_GLOBAL_INTERNAL,
				 bfc.array_type,
				 "data_cells");
  bfc.idx =
    gcc_jit_function_new_local (bfc.func, NULL,
				bfc.int_type,
				"idx");

  gcc_jit_block_add_comment (bfc.curblock,
			     NULL,
			     "idx = 0;");
  gcc_jit_block_add_assignment (bfc.curblock,
				NULL,
				bfc.idx,
				bfc.int_zero);

  bfc.num_open_parens = 0;

  while ( EOF != (ch = fgetc (f_in)))
    bf_compile_char (&bfc, (unsigned char)ch);

  gcc_jit_block_end_with_return (bfc.curblock, NULL, bfc.int_zero);

  fclose (f_in);

  return bfc.ctxt;
}

/* Entrypoint to the compiler.  */

int
main (int argc, char **argv)
{
  const char *input_file;
  const char *output_file;
  gcc_jit_context *ctxt;
  const char *err;

  if (argc != 3)
    {
      fprintf (stderr, "%s: INPUT_FILE OUTPUT_FILE\n", argv[0]);
      return 1;
    }

  input_file = argv[1];
  output_file = argv[2];
  ctxt = bf_compile (input_file);

  gcc_jit_context_compile_to_file (ctxt,
				   GCC_JIT_OUTPUT_KIND_EXECUTABLE,
				   output_file);

  err = gcc_jit_context_get_first_error (ctxt);

  if (err)
    {
      gcc_jit_context_release (ctxt);
      return 1;
    }

  gcc_jit_context_release (ctxt);
  return 0;
}

/* Use the built compiler to compile the example to an executable:

     { dg-jit-set-exe-params SRCDIR/gcc/jit/docs/examples/emit-alphabet.bf emit-alphabet.bf.exe }

   Then run the executable, and verify that it emits the alphabet:

     { dg-final { jit-run-executable emit-alphabet.bf.exe "ABCDEFGHIJKLMNOPQRSTUVWXYZ" } } */
