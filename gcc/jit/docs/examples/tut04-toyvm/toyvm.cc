/* A simple stack-based virtual machine to demonstrate
   JIT-compilation.
   Copyright (C) 2014-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dejagnu.h>

#include <libgccjit++.h>

/* Wrapper around a gcc_jit_result *.  */

class compilation_result
{
public:
  compilation_result (gcc_jit_result *result) :
    m_result (result)
  {
  }
  ~compilation_result ()
  {
    gcc_jit_result_release (m_result);
  }

  void *get_code (const char *funcname)
  {
    return gcc_jit_result_get_code (m_result, funcname);
  }

private:
  gcc_jit_result *m_result;
};

/* Functions are compiled to this function ptr type.  */
typedef int (*toyvm_compiled_func) (int);

enum opcode {
  /* Ops taking no operand.  */
  DUP,
  ROT,
  BINARY_ADD,
  BINARY_SUBTRACT,
  BINARY_MULT,
  BINARY_COMPARE_LT,
  RECURSE,
  RETURN,

  /* Ops taking an operand.  */
  PUSH_CONST,
  JUMP_ABS_IF_TRUE
};

#define FIRST_UNARY_OPCODE (PUSH_CONST)

const char * const opcode_names[] = {
  "DUP",
  "ROT",
  "BINARY_ADD",
  "BINARY_SUBTRACT",
  "BINARY_MULT",
  "BINARY_COMPARE_LT",
  "RECURSE",
  "RETURN",

  "PUSH_CONST",
  "JUMP_ABS_IF_TRUE",
};

struct toyvm_op
{
  /* Which operation.  */
  enum opcode op_opcode;

  /* Some opcodes take an argument.  */
  int op_operand;

  /* The line number of the operation within the source file.  */
  int op_linenum;
};

#define MAX_OPS  (64)

class toyvm_function
{
public:
  void
  add_op (enum opcode opcode,
          int operand, int linenum);

  void
  add_unary_op (enum opcode opcode,
                const char *rest_of_line, int linenum);

  static toyvm_function *
  parse (const char *filename, const char *name);

  void
  disassemble_op (toyvm_op *op, int index, FILE *out);

  void
  disassemble (FILE *out);

  int
  interpret (int arg, FILE *trace);

  compilation_result
  compile ();

  const char *
  get_function_name () const { return m_funcname; }

private:
  void
  make_function_name (const char *filename);

private:
  const char *fn_filename;
  char       *m_funcname;
  int         fn_num_ops;
  toyvm_op    fn_ops[MAX_OPS];
  friend struct compilation_state;
};

#define MAX_STACK_DEPTH (8)

class toyvm_frame
{
public:
  void push (int arg);
  int pop ();
  void dump_stack (FILE *out);

private:
  toyvm_function *frm_function;
  int             frm_pc;
  int             frm_stack[MAX_STACK_DEPTH];
  int             frm_cur_depth;

  friend int toyvm_function::interpret (int arg, FILE *trace);

};

void
toyvm_function::add_op (enum opcode opcode,
                        int operand, int linenum)
{
  toyvm_op *op;
  assert (fn_num_ops < MAX_OPS);
  op = &fn_ops[fn_num_ops++];
  op->op_opcode = opcode;
  op->op_operand = operand;
  op->op_linenum = linenum;
}

void
toyvm_function::add_unary_op (enum opcode opcode,
                              const char *rest_of_line, int linenum)
{
  int operand = atoi (rest_of_line);
  add_op (opcode, operand, linenum);
}

void
toyvm_function::make_function_name (const char *filename)
{
  /* Skip any path separators.  */
  const char *pathsep = strrchr (filename, '/');
  if (pathsep)
    filename = pathsep + 1;

  /* Copy filename to funcname.  */
  m_funcname = (char *)malloc (strlen (filename) + 1);

  strcpy (m_funcname, filename);

  /* Convert "." to NIL terminator.  */
  *(strchr (m_funcname, '.')) = '\0';
}

toyvm_function *
toyvm_function::parse (const char *filename, const char *name)
{
  FILE *f = NULL;
  toyvm_function *fn = NULL;
  char *line = NULL;
  ssize_t linelen;
  size_t bufsize;
  int linenum = 0;

  assert (filename);
  assert (name);

  f = fopen (filename, "r");
  if (!f)
    {
      fprintf (stderr,
	       "cannot open file %s: %s\n",
	       filename, strerror (errno));
      goto error;
    }

  fn = (toyvm_function *)calloc (1, sizeof (toyvm_function));
  if (!fn)
    {
      fprintf (stderr, "out of memory allocating toyvm_function\n");
      goto error;
    }
  fn->fn_filename = filename;
  fn->make_function_name (filename);

  /* Read the lines of the file.  */
  while ((linelen = getline (&line, &bufsize, f)) != -1)
    {
      /* Note that this is a terrible parser, but it avoids the need to
	 bring in lex/yacc as a dependency.  */
      linenum++;

      if (0)
	fprintf (stdout, "%3d: %s", linenum, line);

      /* Lines beginning with # are comments.  */
      if (line[0] == '#')
	continue;

      /* Skip blank lines.  */
      if (line[0] == '\n')
	continue;

#define LINE_MATCHES(OPCODE) (0 == strncmp ((OPCODE), line, strlen (OPCODE)))
      if (LINE_MATCHES ("DUP\n"))
	fn->add_op (DUP, 0, linenum);
      else if (LINE_MATCHES ("ROT\n"))
	fn->add_op (ROT, 0, linenum);
      else if (LINE_MATCHES ("BINARY_ADD\n"))
	fn->add_op (BINARY_ADD, 0, linenum);
      else if (LINE_MATCHES ("BINARY_SUBTRACT\n"))
	fn->add_op (BINARY_SUBTRACT, 0, linenum);
      else if (LINE_MATCHES ("BINARY_MULT\n"))
	fn->add_op (BINARY_MULT, 0, linenum);
      else if (LINE_MATCHES ("BINARY_COMPARE_LT\n"))
	fn->add_op (BINARY_COMPARE_LT, 0, linenum);
      else if (LINE_MATCHES ("RECURSE\n"))
	fn->add_op (RECURSE, 0, linenum);
      else if (LINE_MATCHES ("RETURN\n"))
	fn->add_op (RETURN, 0, linenum);
      else if (LINE_MATCHES ("PUSH_CONST "))
	fn->add_unary_op (PUSH_CONST,
                          line + strlen ("PUSH_CONST "), linenum);
      else if (LINE_MATCHES ("JUMP_ABS_IF_TRUE "))
	fn->add_unary_op (JUMP_ABS_IF_TRUE,
                          line + strlen("JUMP_ABS_IF_TRUE "), linenum);
      else
	{
	  fprintf (stderr, "%s:%d: parse error\n", filename, linenum);
	  free (fn);
	  fn = NULL;
	  goto error;
	}
#undef LINE_MATCHES
    }
  free (line);
  fclose (f);

  return fn;

 error:
  free (line);
  if (f)
    fclose (f);
  free (fn);
  return NULL;
}

void
toyvm_function::disassemble_op (toyvm_op *op, int index, FILE *out)
{
  fprintf (out, "%s:%d: index %d: %s",
	   fn_filename, op->op_linenum, index,
	   opcode_names[op->op_opcode]);
  if (op->op_opcode >= FIRST_UNARY_OPCODE)
    fprintf (out, " %d", op->op_operand);
  fprintf (out, "\n");
}

void
toyvm_function::disassemble (FILE *out)
{
  int i;
  for (i = 0; i < fn_num_ops; i++)
    {
      toyvm_op *op = &fn_ops[i];
      disassemble_op (op, i, out);
    }
}

void
toyvm_frame::push (int arg)
{
  assert (frm_cur_depth < MAX_STACK_DEPTH);
  frm_stack[frm_cur_depth++] = arg;
}

int
toyvm_frame::pop ()
{
  assert (frm_cur_depth > 0);
  return frm_stack[--frm_cur_depth];
}

void
toyvm_frame::dump_stack (FILE *out)
{
  int i;
  fprintf (out, "stack:");
  for (i = 0; i < frm_cur_depth; i++)
    {
      fprintf (out, " %d", frm_stack[i]);
    }
  fprintf (out, "\n");
}

/* Execute the given function.  */

int
toyvm_function::interpret (int arg, FILE *trace)
{
  toyvm_frame frame;
#define PUSH(ARG) (frame.push (ARG))
#define POP(ARG) (frame.pop ())

  frame.frm_function = this;
  frame.frm_pc = 0;
  frame.frm_cur_depth = 0;

  PUSH (arg);

  while (1)
    {
      toyvm_op *op;
      int x, y;
      assert (frame.frm_pc < fn_num_ops);
      op = &fn_ops[frame.frm_pc++];

      if (trace)
	{
	  frame.dump_stack (trace);
	  disassemble_op (op, frame.frm_pc, trace);
	}

      switch (op->op_opcode)
	{
	  /* Ops taking no operand.  */
	case DUP:
	  x = POP ();
	  PUSH (x);
	  PUSH (x);
	  break;

	case ROT:
	  y = POP ();
	  x = POP ();
	  PUSH (y);
	  PUSH (x);
	  break;

	case BINARY_ADD:
	  y = POP ();
	  x = POP ();
	  PUSH (x + y);
	  break;

	case BINARY_SUBTRACT:
	  y = POP ();
	  x = POP ();
	  PUSH (x - y);
	  break;

	case BINARY_MULT:
	  y = POP ();
	  x = POP ();
	  PUSH (x * y);
	  break;

	case BINARY_COMPARE_LT:
	  y = POP ();
	  x = POP ();
	  PUSH (x < y);
	  break;

	case RECURSE:
	  x = POP ();
	  x = interpret (x, trace);
	  PUSH (x);
	  break;

	case RETURN:
	  return POP ();

	  /* Ops taking an operand.  */
	case PUSH_CONST:
	  PUSH (op->op_operand);
	  break;

	case JUMP_ABS_IF_TRUE:
	  x = POP ();
	  if (x)
	    frame.frm_pc = op->op_operand;
	  break;

	default:
	  assert (0); /* unknown opcode */

	} /* end of switch on opcode */
    } /* end of while loop */

#undef PUSH
#undef POP
}

/* JIT compilation.  */

class compilation_state
{
public:
  compilation_state (toyvm_function &toyvmfn) :
    toyvmfn (toyvmfn)
  {}

  void create_context ();
  void create_types ();
  void create_locations ();
  void create_function (const char *funcname);
  compilation_result compile ();

private:
  void
  add_push (gccjit::block block,
            gccjit::rvalue rvalue,
            gccjit::location loc);

  void
  add_pop (gccjit::block block,
           gccjit::lvalue lvalue,
           gccjit::location loc);

private:

  /* State.  */

  toyvm_function &toyvmfn;

  gccjit::context ctxt;

  gccjit::type int_type;
  gccjit::type bool_type;
  gccjit::type stack_type; /* int[MAX_STACK_DEPTH] */

  gccjit::rvalue const_one;

  gccjit::function fn;
  gccjit::param param_arg;
  gccjit::lvalue stack;
  gccjit::lvalue stack_depth;
  gccjit::lvalue x;
  gccjit::lvalue y;

  gccjit::location op_locs[MAX_OPS];
  gccjit::block initial_block;
  gccjit::block op_blocks[MAX_OPS];

};

/* The main compilation hook.  */

compilation_result
toyvm_function::compile ()
{
  compilation_state state (*this);

  state.create_context ();
  state.create_types ();
  state.create_locations ();
  state.create_function (get_function_name ());

  /* We've now finished populating the context.  Compile it.  */
  return state.compile ();
}

/* Stack manipulation.  */

void
compilation_state::add_push (gccjit::block block,
                             gccjit::rvalue rvalue,
                             gccjit::location loc)
{
  /* stack[stack_depth] = RVALUE */
  block.add_assignment (
    /* stack[stack_depth] */
    ctxt.new_array_access (
      stack,
      stack_depth,
      loc),
    rvalue,
    loc);

  /* "stack_depth++;".  */
  block.add_assignment_op (
    stack_depth,
    GCC_JIT_BINARY_OP_PLUS,
    const_one,
    loc);
}

void
compilation_state::add_pop (gccjit::block block,
                            gccjit::lvalue lvalue,
                            gccjit::location loc)
{
  /* "--stack_depth;".  */
  block.add_assignment_op (
    stack_depth,
    GCC_JIT_BINARY_OP_MINUS,
    const_one,
    loc);

  /* "LVALUE = stack[stack_depth];".  */
  block.add_assignment (
    lvalue,
    /* stack[stack_depth] */
    ctxt.new_array_access (stack,
                           stack_depth,
                           loc),
    loc);
}

/* Create the context. */

void
compilation_state::create_context ()
{
  ctxt = gccjit::context::acquire ();

  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE,
                              0);
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE,
                              0);
  ctxt.set_int_option (GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL,
                             3);
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES,
                              0);
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING,
                              0);
  ctxt.set_bool_option (GCC_JIT_BOOL_OPTION_DEBUGINFO,
                              1);
}

/* Create types.  */

void
compilation_state::create_types ()
{
  /* Create types.  */
  int_type = ctxt.get_type (GCC_JIT_TYPE_INT);
  bool_type = ctxt.get_type (GCC_JIT_TYPE_BOOL);
  stack_type = ctxt.new_array_type (int_type, MAX_STACK_DEPTH);

  /* The constant value 1.  */
  const_one = ctxt.one (int_type);

}

/* Create locations.  */

void
compilation_state::create_locations ()
{
  for (int pc = 0; pc < toyvmfn.fn_num_ops; pc++)
    {
      toyvm_op *op = &toyvmfn.fn_ops[pc];

      op_locs[pc] = ctxt.new_location (toyvmfn.fn_filename,
                                       op->op_linenum,
                                       0); /* column */
    }
}

/* Creating the function.  */

void
compilation_state::create_function (const char *funcname)
{
  std::vector <gccjit::param> params;
  param_arg = ctxt.new_param (int_type, "arg", op_locs[0]);
  params.push_back (param_arg);
  fn = ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                          int_type,
                          funcname,
                          params, 0,
                          op_locs[0]);

  /* Create stack lvalues.  */
  stack = fn.new_local (stack_type, "stack");
  stack_depth = fn.new_local (int_type, "stack_depth");
  x = fn.new_local (int_type, "x");
  y = fn.new_local (int_type, "y");

  /* 1st pass: create blocks, one per opcode. */

  /* We need an entry block to do one-time initialization, so create that
     first.  */
  initial_block = fn.new_block ("initial");

  /* Create a block per operation.  */
  for (int pc = 0; pc < toyvmfn.fn_num_ops; pc++)
    {
      char buf[16];
      sprintf (buf, "instr%i", pc);
      op_blocks[pc] = fn.new_block (buf);
    }

  /* Populate the initial block.  */

  /* "stack_depth = 0;".  */
  initial_block.add_assignment (stack_depth,
                                ctxt.zero (int_type),
                                op_locs[0]);

  /* "PUSH (arg);".  */
  add_push (initial_block,
	    param_arg,
            op_locs[0]);

  /* ...and jump to insn 0.  */
  initial_block.end_with_jump (op_blocks[0],
                               op_locs[0]);

  /* 2nd pass: fill in instructions.  */
  for (int pc = 0; pc < toyvmfn.fn_num_ops; pc++)
    {
      gccjit::location loc = op_locs[pc];

      gccjit::block block = op_blocks[pc];
      gccjit::block next_block = (pc < toyvmfn.fn_num_ops
                                  ? op_blocks[pc + 1]
                                  : NULL);

      toyvm_op *op;
      op = &toyvmfn.fn_ops[pc];

      /* Helper macros.  */

#define X_EQUALS_POP()\
      add_pop (block, x, loc)
#define Y_EQUALS_POP()\
      add_pop (block, y, loc)
#define PUSH_RVALUE(RVALUE)\
      add_push (block, (RVALUE), loc)
#define PUSH_X()\
      PUSH_RVALUE (x)
#define PUSH_Y() \
      PUSH_RVALUE (y)

      block.add_comment (opcode_names[op->op_opcode], loc);

      /* Handle the individual opcodes.  */

      switch (op->op_opcode)
	{
	case DUP:
	  X_EQUALS_POP ();
	  PUSH_X ();
	  PUSH_X ();
	  break;

	case ROT:
	  Y_EQUALS_POP ();
	  X_EQUALS_POP ();
	  PUSH_Y ();
	  PUSH_X ();
	  break;

	case BINARY_ADD:
	  Y_EQUALS_POP ();
	  X_EQUALS_POP ();
	  PUSH_RVALUE (
	   ctxt.new_binary_op (
	     GCC_JIT_BINARY_OP_PLUS,
	     int_type,
             x, y,
             loc));
	  break;

	case BINARY_SUBTRACT:
	  Y_EQUALS_POP ();
	  X_EQUALS_POP ();
	  PUSH_RVALUE (
           ctxt.new_binary_op (
	     GCC_JIT_BINARY_OP_MINUS,
	     int_type,
             x, y,
             loc));
	  break;

	case BINARY_MULT:
	  Y_EQUALS_POP ();
	  X_EQUALS_POP ();
	  PUSH_RVALUE (
           ctxt.new_binary_op (
	     GCC_JIT_BINARY_OP_MULT,
	     int_type,
             x, y,
             loc));
	  break;

	case BINARY_COMPARE_LT:
	  Y_EQUALS_POP ();
	  X_EQUALS_POP ();
	  PUSH_RVALUE (
	     /* cast of bool to int */
	     ctxt.new_cast (
	       /* (x < y) as a bool */
	       ctxt.new_comparison (
		 GCC_JIT_COMPARISON_LT,
                 x, y,
                 loc),
	       int_type,
               loc));
	  break;

	case RECURSE:
	  {
	    X_EQUALS_POP ();
	    PUSH_RVALUE (
	      ctxt.new_call (
		fn,
		x,
                loc));
	    break;
	  }

	case RETURN:
	  X_EQUALS_POP ();
	  block.end_with_return (x, loc);
	  break;

	  /* Ops taking an operand.  */
	case PUSH_CONST:
	  PUSH_RVALUE (
	    ctxt.new_rvalue (int_type, op->op_operand));
	  break;

	case JUMP_ABS_IF_TRUE:
	  X_EQUALS_POP ();
	  block.end_with_conditional (
	    /* "(bool)x".  */
            ctxt.new_cast (x, bool_type, loc),
	    op_blocks[op->op_operand], /* on_true */
	    next_block, /* on_false */
            loc); 
	  break;

	default:
	  assert(0);
	} /* end of switch on opcode */

      /* Go to the next block.  */
      if (op->op_opcode != JUMP_ABS_IF_TRUE
	  && op->op_opcode != RETURN)
	block.end_with_jump (next_block, loc);

    } /* end of loop on PC locations.  */
}

compilation_result
compilation_state::compile ()
{
  return ctxt.compile ();
}

char test[1024];

#define CHECK_NON_NULL(PTR) \
  do {                                       \
    if ((PTR) != NULL)                       \
      {                                      \
	pass ("%s: %s is non-null", test, #PTR); \
      }                                      \
    else                                     \
      {                                      \
	fail ("%s: %s is NULL", test, #PTR); \
	abort ();                            \
    }                                        \
  } while (0)

#define CHECK_VALUE(ACTUAL, EXPECTED) \
  do {                                       \
    if ((ACTUAL) == (EXPECTED))              \
      {                                      \
	pass ("%s: actual: %s == expected: %s", test, #ACTUAL, #EXPECTED); \
      }                                      \
    else                                     \
      {                                        \
	fail ("%s: actual: %s != expected: %s", test, #ACTUAL, #EXPECTED); \
	fprintf (stderr, "incorrect value\n"); \
	abort ();                              \
    }                                        \
  } while (0)

static void
test_script (const char *scripts_dir, const char *script_name, int input,
	     int expected_result)
{
  char *script_path;
  toyvm_function *fn;
  int interpreted_result;
  toyvm_compiled_func code;
  int compiled_result;

  snprintf (test, sizeof (test), "toyvm.cc: %s", script_name);

  script_path = (char *)malloc (strlen (scripts_dir)
				+ strlen (script_name) + 1);
  CHECK_NON_NULL (script_path);
  sprintf (script_path, "%s%s", scripts_dir, script_name);

  fn = toyvm_function::parse (script_path, script_name);
  CHECK_NON_NULL (fn);

  interpreted_result = fn->interpret (input, NULL);
  CHECK_VALUE (interpreted_result, expected_result);

  compilation_result compiler_result = fn->compile ();

  const char *funcname = fn->get_function_name ();
  code = (toyvm_compiled_func)compiler_result.get_code (funcname);
  CHECK_NON_NULL (code);

  compiled_result = code (input);
  CHECK_VALUE (compiled_result, expected_result);

  free (script_path);
}

#define PATH_TO_SCRIPTS  ("/jit/docs/examples/tut04-toyvm/")

static void
test_suite (void)
{
  const char *srcdir;
  char *scripts_dir;

  snprintf (test, sizeof (test), "toyvm.cc");

  /* We need to locate the test scripts.
     Rely on "srcdir" being set in the environment.  */

  srcdir = getenv ("srcdir");
  CHECK_NON_NULL (srcdir);

  scripts_dir = (char *)malloc (strlen (srcdir) + strlen(PATH_TO_SCRIPTS)
				+ 1);
  CHECK_NON_NULL (scripts_dir);
  sprintf (scripts_dir, "%s%s", srcdir, PATH_TO_SCRIPTS);

  test_script (scripts_dir, "factorial.toy", 10, 3628800);
  test_script (scripts_dir, "fibonacci.toy", 10, 55);

  free (scripts_dir);
}

int
main (int argc, char **argv)
{
  const char *filename = NULL;
  toyvm_function *fn = NULL;

  /* If called with no args, assume we're being run by the test suite.  */
  if (argc < 3)
    {
      test_suite ();
      return 0;
    }

  if (argc != 3)
    {
      fprintf (stdout,
	"%s FILENAME INPUT: Parse and run a .toy file\n",
	argv[0]);
      exit (1);
    }

  filename = argv[1];
  fn = toyvm_function::parse (filename, filename);
  if (!fn)
    exit (1);

  if (0)
    fn->disassemble (stdout);

  printf ("interpreter result: %d\n",
	  fn->interpret (atoi (argv[2]), NULL));

  /* JIT-compilation.  */
  compilation_result compiler_result = fn->compile ();

  const char *funcname = fn->get_function_name ();
  toyvm_compiled_func code
    = (toyvm_compiled_func)compiler_result.get_code (funcname);

  printf ("compiler result: %d\n",
	  code (atoi (argv[2])));

 return 0;
}
