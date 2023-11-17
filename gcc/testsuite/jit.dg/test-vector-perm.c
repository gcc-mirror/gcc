#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

typedef int v4si __attribute__ ((vector_size (16)));

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *v4si =
    gcc_jit_type_get_vector (int_type, 4);

  gcc_jit_function *func_vector =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "vector",
				  0, NULL,
				  0);

  gcc_jit_rvalue *elements[4]
    = { gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 3),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 4),
    };

  gcc_jit_rvalue *vector
    = gcc_jit_context_new_rvalue_from_vector (ctxt, NULL, v4si, 4, elements);

  gcc_jit_rvalue *index
    = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2);

  gcc_jit_block *block_a = gcc_jit_function_new_block (func_vector, NULL);
  gcc_jit_lvalue *value
    = gcc_jit_context_new_vector_access (ctxt, NULL, vector, index);
  gcc_jit_block_end_with_return (block_a, NULL, gcc_jit_lvalue_as_rvalue (value));

  gcc_jit_function *func_vector_perm =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  v4si,
				  "vector_perm",
				  0, NULL,
				  0);

  gcc_jit_rvalue *elements2[4]
    = { gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 5),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 6),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 7),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 8),
    };

  gcc_jit_rvalue *vector2
    = gcc_jit_context_new_rvalue_from_vector (ctxt, NULL, v4si, 4, elements2);

  gcc_jit_rvalue *mask_values[4]
    = { gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 7),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 3),
    gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0),
    };

  gcc_jit_rvalue *mask
    = gcc_jit_context_new_rvalue_from_vector (ctxt, NULL, v4si, 4, mask_values);

  gcc_jit_block *block_b = gcc_jit_function_new_block (func_vector_perm, NULL);
  gcc_jit_rvalue *result = gcc_jit_context_new_rvalue_vector_perm (ctxt, NULL, vector, vector2, mask);
  gcc_jit_block_end_with_return (block_b, NULL, result);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  typedef int (*vector) ();
  vector fn = (vector)gcc_jit_result_get_code (result, "vector");
  CHECK_NON_NULL (fn);

  int vector_access = fn ();
  CHECK_VALUE (vector_access, 3);

  typedef v4si (*vector_perm) ();
  vector_perm perm_fn = (vector_perm)gcc_jit_result_get_code (result, "vector_perm");
  CHECK_NON_NULL (perm_fn);

  v4si vector_perm_res = perm_fn ();
  v4si expected_vec = { 8, 3, 4, 1 };
  CHECK_VECTOR_VALUE (4, vector_perm_res, expected_vec);
}
