#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct assignable_struct
{
  int a;
  char b;
  float c;
};

union assignable_union
{
  int a;
  char b;
  float c;
};

/* Verify that compound assignment works; let's try to inject the
   equivalent of:

     struct assignable_struct
     test_struct_assignment (struct assignable_struct x)
     {
       struct assignable_struct y, z;
       y = x;
       z = y;
       return z;
     }

   and the same, for "union assignable_union".  */

/* Make the type "struct assignable_struct" or "union assignable_union".  */

static gcc_jit_type *
make_type (gcc_jit_context *ctxt, int make_union)
{
  gcc_jit_type *t_int =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *t_char =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *t_float =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT);

  gcc_jit_field *a =
    gcc_jit_context_new_field (ctxt,
			       NULL,
			       t_int,
			       "a");
  gcc_jit_field *b =
    gcc_jit_context_new_field (ctxt,
			       NULL,
			       t_char,
			       "b");
  gcc_jit_field *c =
    gcc_jit_context_new_field (ctxt,
			       NULL,
			       t_float,
			       "c");
  gcc_jit_field *fields[] = {a, b, c};
  if (make_union)
      return gcc_jit_context_new_union_type (ctxt, NULL,
					     "assignable_union",
					     3, fields);
  else
    return gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt, NULL,
				       "assignable_struct",
				       3, fields));
}

static void
make_function (gcc_jit_context *ctxt, int make_union, const char *funcname)
{
  gcc_jit_type *test_type = make_type (ctxt, make_union);
  gcc_jit_param *x =
    gcc_jit_context_new_param (ctxt, NULL,
			       test_type, "x");
  gcc_jit_function *fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  test_type,
				  funcname,
				  1, &x,
				  0);
  gcc_jit_lvalue *y =
    gcc_jit_function_new_local (fn, NULL, test_type, "y");
  gcc_jit_lvalue *z =
    gcc_jit_function_new_local (fn, NULL, test_type, "z");
  gcc_jit_block *block =
    gcc_jit_function_new_block (fn, NULL);
  gcc_jit_block_add_assignment (block, NULL,
				y, gcc_jit_param_as_rvalue (x));
  gcc_jit_block_add_assignment (block, NULL,
				z, gcc_jit_lvalue_as_rvalue (y));
  gcc_jit_block_end_with_return (block, NULL,
				 gcc_jit_lvalue_as_rvalue (z));
}

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  make_function (ctxt, 0, "test_struct_assignment");
  make_function (ctxt, 1, "test_union_assignment");
}

static void
verify_test_struct_assignment (gcc_jit_result *result)
{
  typedef struct assignable_struct (*fn_type) (struct assignable_struct);
  fn_type test_struct_assignment =
    (fn_type)gcc_jit_result_get_code (result, "test_struct_assignment");
  CHECK_NON_NULL (test_struct_assignment);

  struct assignable_struct s, t;
  s.a = 500;
  s.b = 'A';
  s.c = 1.0;
  t = test_struct_assignment (s);
  CHECK_VALUE (t.a, 500);
  CHECK_VALUE (t.b, 'A');
  CHECK_VALUE (t.c, 1.0);
}

static void
verify_test_union_assignment (gcc_jit_result *result)
{
  typedef union assignable_union (*fn_type) (union assignable_union);
  fn_type test_union_assignment =
    (fn_type)gcc_jit_result_get_code (result, "test_union_assignment");
  CHECK_NON_NULL (test_union_assignment);

  union assignable_union p, q;

  p.a = 500;
  q = test_union_assignment (p);
  CHECK_VALUE (q.a, 500);

  p.b = 'A';
  q = test_union_assignment (p);
  CHECK_VALUE (q.b, 'A');

  p.c = 1.0;
  q = test_union_assignment (p);
  CHECK_VALUE (q.c, 1.0);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  verify_test_struct_assignment (result);
  verify_test_union_assignment (result);
}
