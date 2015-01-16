/* Fuzz-testing of libgccjit API.
   Currently this triggers internal compiler errors, typically due to type
   mismatches.  */
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "libgccjit.h"

#define TEST_PROVIDES_MAIN
#include "harness.h"

typedef struct fuzzer
{
  gcc_jit_context *ctxt;

  unsigned int seed;

  int num_types;
  gcc_jit_type **types;

  int num_globals;
  gcc_jit_lvalue **globals;

  int num_funcs;
  gcc_jit_function **funcs;

} fuzzer;

static void
fuzzer_init (fuzzer *f, gcc_jit_context *ctxt, unsigned int seed);

static int
fuzzer_randrange (fuzzer *f, int min, int max);

static gcc_jit_location *
get_random_location (fuzzer *f);

static gcc_jit_type *
get_random_type (fuzzer *f);

static gcc_jit_type *
make_random_type (fuzzer *f);

static gcc_jit_lvalue *
make_random_global (fuzzer *f);

static gcc_jit_function *
make_random_function (fuzzer *f);

typedef struct function_fuzzer
{
  fuzzer *f;

  int num_params;
  gcc_jit_param **params;

  gcc_jit_function *fn;

  int num_locals;
  gcc_jit_lvalue **locals;

  gcc_jit_block *block;

} function_fuzzer;

static void
function_fuzzer_add_stmt (function_fuzzer *ff);

static gcc_jit_lvalue *
get_random_lvalue (function_fuzzer *ff, int max_depth);

static gcc_jit_rvalue *
get_random_rvalue (function_fuzzer *ff, int max_depth);

/* fuzzer defns.  */

static void
fuzzer_init (fuzzer *f, gcc_jit_context *ctxt, unsigned int seed)
{
  int i;
  memset (f, 0, sizeof (*f));
  f->ctxt = ctxt;
  f->seed = seed;

  int num_types = fuzzer_randrange (f, 5, 10);
  f->types = malloc (num_types * sizeof (gcc_jit_type *));

  int num_funcs = fuzzer_randrange (f, 3, 5);
  f->funcs = malloc (num_funcs * sizeof (gcc_jit_function *));

  int num_globals = fuzzer_randrange (f, 5, 10);
  f->globals = malloc (num_globals * sizeof (gcc_jit_lvalue *));

  for (i = 0; i < num_types; i++)
    {
      gcc_jit_type *type = make_random_type (f);
      assert (type);
      f->types[f->num_types++] = type;
    }

  for (i = 0; i < num_globals; i++)
    f->globals[f->num_globals++] = make_random_global (f);

  for (i = 0; i < num_funcs; i++)
    f->funcs[f->num_funcs++] = make_random_function (f);

  /* Now clean out f.  */
  free (f->types);
  free (f->funcs);
  free (f->globals);
}

/* Get random int in inclusive range [min, max].  */

static int fuzzer_randrange (fuzzer *f, int min, int max)
{
  assert (min <= max);
  int i = rand_r (&f->seed);
  int result = (i % (max + 1 - min)) + min;
  assert (result >= min);
  assert (result <= max);
  return result;
}

static gcc_jit_location *
get_random_location (fuzzer *f)
{
  const char *filename = NULL;

  if (fuzzer_randrange (f, 0, 1))
    return NULL;

  switch (fuzzer_randrange (f, 1, 2))
    {
    case 1:
      filename = "foo.c";
      break;
    case 2:
      filename = "bar.c";
      break;
    }

  return gcc_jit_context_new_location (f->ctxt,
				       filename,
				       fuzzer_randrange (f, 1, 1000),
				       fuzzer_randrange (f, 1, 1000));
}

const enum gcc_jit_types types[] = {
  GCC_JIT_TYPE_VOID,

  GCC_JIT_TYPE_VOID_PTR,

  GCC_JIT_TYPE_CHAR,
  GCC_JIT_TYPE_SIGNED_CHAR,
  GCC_JIT_TYPE_UNSIGNED_CHAR,

  GCC_JIT_TYPE_SHORT,
  GCC_JIT_TYPE_UNSIGNED_SHORT,

  GCC_JIT_TYPE_INT,
  GCC_JIT_TYPE_UNSIGNED_INT,

  GCC_JIT_TYPE_LONG,
  GCC_JIT_TYPE_UNSIGNED_LONG,

  GCC_JIT_TYPE_LONG_LONG,
  GCC_JIT_TYPE_UNSIGNED_LONG_LONG,

  GCC_JIT_TYPE_FLOAT,
  GCC_JIT_TYPE_DOUBLE,
  GCC_JIT_TYPE_LONG_DOUBLE,

  GCC_JIT_TYPE_CONST_CHAR_PTR,

  GCC_JIT_TYPE_SIZE_T,

  GCC_JIT_TYPE_FILE_PTR
};
#define NUM_TYPES (sizeof(types)/sizeof(types[0]))

static gcc_jit_type *
get_random_type (fuzzer *f)
{
  int i = fuzzer_randrange (f, 0, (NUM_TYPES - 1) + f->num_types);
  if (i < NUM_TYPES)
    return gcc_jit_context_get_type (f->ctxt, types[i]);
  assert ((i - NUM_TYPES) < f->num_types);
  assert (f->types[i - NUM_TYPES]);
  return f->types[i - NUM_TYPES];
}

static gcc_jit_type *
make_random_type (fuzzer *f)
{
  switch (fuzzer_randrange (f, 0, 5))
    {
    case 0:
      return gcc_jit_type_get_pointer (get_random_type (f));
    case 1:
      return gcc_jit_type_get_const (get_random_type (f));
    default:
      {
	/* Create a struct.  */
	int num_fields = fuzzer_randrange (f, 0, 10);
	gcc_jit_field **fields = \
	  malloc (num_fields * sizeof (gcc_jit_field *));
	int i;
	for (i = 0; i < num_fields ; i++)
	  {
	    char field_name[256];
	    sprintf (field_name, "field%i", i);
	    fields[i] = gcc_jit_context_new_field (f->ctxt,
						   get_random_location (f),
						   get_random_type (f),
						   field_name);
	  }
	char struct_name[256];
	sprintf (struct_name, "s%i", f->num_types);
	gcc_jit_struct *struct_ = \
	  gcc_jit_context_new_struct_type (f->ctxt,
					   get_random_location (f),
					   struct_name,
					   num_fields,
					   fields);
	free (fields);
	return gcc_jit_struct_as_type (struct_);
      }
    }
}

static gcc_jit_lvalue *
make_random_global (fuzzer *f)
{
  char global_name[256];
  sprintf (global_name, "g%i", f->num_globals);
  return gcc_jit_context_new_global (f->ctxt,
				     get_random_location (f),
				     GCC_JIT_GLOBAL_EXPORTED,
				     get_random_type (f),
				     global_name);
}

static gcc_jit_function *
make_random_function (fuzzer *f)
{
  char func_name[256];
  sprintf (func_name, "fn%i", f->num_funcs);

  function_fuzzer *ff = malloc (sizeof (function_fuzzer));
  memset (ff, 0, sizeof (*ff));

  ff->f = f;

  ff->num_params = fuzzer_randrange (f, 0, 10);
  ff->params = malloc (ff->num_params * sizeof (gcc_jit_param *));
  int i;
  for (i = 0; i < ff->num_params; i++)
    {
      char param_name[256];
      sprintf (param_name, "param%i", i);
      ff->params[i] = \
	gcc_jit_context_new_param (f->ctxt,
				   get_random_location (f),
				   get_random_type (f),
				   param_name);
    }

  enum gcc_jit_function_kind kind =
    ((enum gcc_jit_function_kind)
     fuzzer_randrange (f, 0, GCC_JIT_FUNCTION_IMPORTED));

  ff->fn = \
    gcc_jit_context_new_function (
      f->ctxt,
      get_random_location (f),
      kind,
      get_random_type (f),
      func_name,
      ff->num_params,
      ff->params,
      fuzzer_randrange (f, 0, 1));
  ff->block = gcc_jit_function_new_block (ff->fn, NULL);

  /* Create locals.  */
  if (kind != GCC_JIT_FUNCTION_IMPORTED)
    {
      ff->num_locals = fuzzer_randrange (f, 0, 10);
      ff->locals = malloc (ff->num_locals * sizeof (gcc_jit_lvalue *));
      for (i = 0; i < ff->num_locals; i++)
	{
	  char local_name[256];
	  sprintf (local_name, "local%i", i);
	  ff->locals[i] =
	    gcc_jit_function_new_local (ff->fn,
					get_random_location (f),
					get_random_type (f),
					local_name);
	}
    }
  /* TODO: use locals.  */

  if (kind != GCC_JIT_FUNCTION_IMPORTED)
    {
      /* TODO: create body */
      int num_stmts = fuzzer_randrange (f, 0, 10);
      for (i = 0; i < num_stmts; i++)
	function_fuzzer_add_stmt (ff);
    }

  gcc_jit_block_end_with_return (ff->block, NULL, get_random_rvalue (ff, 3));


  gcc_jit_function *result = ff->fn;

  free (ff->locals);
  free (ff->params);
  free (ff);

  return result;
}

/* function_fuzzer defns.  */

static void function_fuzzer_add_stmt (function_fuzzer *ff)
{
  gcc_jit_block_add_eval (ff->block,
			  get_random_location (ff->f),
			  get_random_rvalue (ff, 4));
  gcc_jit_block_add_assignment (ff->block,
				get_random_location (ff->f),
				get_random_lvalue (ff, 4),
				get_random_rvalue (ff, 4));
  /* TODO: place more kinds of statement */
  /* TODO: labels  */
}

static gcc_jit_lvalue *get_random_lvalue (function_fuzzer *ff, int max_depth)
{
  int choice = fuzzer_randrange (ff->f, 0,
				 ff->num_params
				 + ff->num_locals
				 + ff->f->num_globals - 1);
  if (choice < ff->num_params)
    return gcc_jit_param_as_lvalue (ff->params[choice]);
  choice -= ff->num_params;

  if (choice < ff->num_locals)
    return ff->locals[choice];
  choice -= ff->num_locals;

  assert (choice < ff->f->num_globals);
  return ff->f->globals[choice];
}

static gcc_jit_rvalue *get_random_rvalue (function_fuzzer *ff, int max_depth)
{
  int use_lvalue = fuzzer_randrange (ff->f, 0, 1);
  if (use_lvalue)
    return gcc_jit_lvalue_as_rvalue (get_random_lvalue (ff, max_depth));

  int choice = fuzzer_randrange (ff->f, 0, 1);

  /* Compound op: */
  switch (choice)
    {
    case 0:
      return gcc_jit_context_new_string_literal (ff->f->ctxt, "hello");
    case 1:
      return gcc_jit_context_new_rvalue_from_int (
	ff->f->ctxt,
	get_random_type (ff->f),
	fuzzer_randrange (ff->f, 0, INT_MAX));
    case 2:
      return gcc_jit_context_new_rvalue_from_double (
	ff->f->ctxt,
	get_random_type (ff->f),
	((double)fuzzer_randrange (ff->f, 0, INT_MAX))
	 / (double)fuzzer_randrange (ff->f, 0, INT_MAX));
    case 3:
      return gcc_jit_context_new_unary_op (
	ff->f->ctxt,
	get_random_location (ff->f),
	((enum gcc_jit_unary_op)
	 fuzzer_randrange (ff->f, 0, GCC_JIT_UNARY_OP_LOGICAL_NEGATE)),
	get_random_type (ff->f),
	get_random_rvalue (ff, max_depth - 1));
    case 4:
      return gcc_jit_context_new_binary_op (
	ff->f->ctxt,
	get_random_location (ff->f),
	((enum gcc_jit_binary_op)
	 fuzzer_randrange (ff->f, 0, GCC_JIT_BINARY_OP_LOGICAL_OR)),
	get_random_type (ff->f),
	get_random_rvalue (ff, max_depth - 1),
	get_random_rvalue (ff, max_depth - 1));
    case 5:
      return gcc_jit_lvalue_get_address (
	get_random_lvalue (ff, max_depth - 1),
	get_random_location (ff->f));

      /* TODO:
	 - comparisons
	 - calls
	 - array lookup
	 - fields
	 - dereferencing */
    }
  return NULL;
}


/* Top-level defns for use by harness.	*/
void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  fuzzer f;
  int seed = *(int*)user_data;

  fuzzer_init (&f, ctxt, seed);
}

static int num_completed_compilations = 0;

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* We can make no guarantees about whether we built something
     valid or not, and the result might have an infinite loop,
     so we can't execute it.

     If we survive to reach here, note the fact for DejaGnu.  */
  pass ("%s: survived compilation", test);
  if (result)
    num_completed_compilations++;
}

static void
test_fuzzer (const char *argv0, int seed)
{
  test_jit (argv0, &seed);
}

int
main (int argc, char **argv)
{
  int i, seed;
  const int NUM_ITERATIONS = 2;
  const int NUM_SEEDS = 100;
  for (i = 1; i <= NUM_ITERATIONS; i++)
    {
      for (seed = 0; seed < NUM_SEEDS ; seed++)
	{
	  snprintf (test, sizeof (test),
		    "%s iteration %d of %d; seed %d of %d",
		    extract_progname (argv[0]),
		    i, NUM_ITERATIONS, seed, NUM_SEEDS);
	  test_fuzzer (argv[0], seed);
	}
    }
  pass ("%s: survived running all tests", extract_progname (argv[0]));
  note ("%s: num completed compilations: %d", extract_progname (argv[0]),
	num_completed_compilations);
  totals ();

  return 0;
}
