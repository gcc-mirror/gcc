#include <libgccjit.h>
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Try to inject the equivalent of:
        static int idx;
        void test_func (void)
	{
	  idx += (unsigned char)1; // mismatching type
	}
     and verify that we don't get an ICE inside gimplification
     due to the type mismatch.  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *unsigned_char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_CHAR);
  gcc_jit_function *test_func =
    gcc_jit_context_new_function (ctxt, /* gcc_jit_context *ctxt */
                                  NULL, /* gcc_jit_location *loc */
                                  GCC_JIT_FUNCTION_EXPORTED, /* enum gcc_jit_function_kind kind */
                                  void_type, /* gcc_jit_type *return_type */
                                  "test_func", /* const char *name */
                                  0, /* int num_params */
                                  NULL, /* gcc_jit_param **params */
                                  0); /* int is_variadic */
  gcc_jit_block *block =
    gcc_jit_function_new_block (test_func, "initial");

  gcc_jit_rvalue *unsigned_char_1 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         unsigned_char_type, /* gcc_jit_type *numeric_type */
                                         1); /* int value */
  gcc_jit_lvalue *idx =
    gcc_jit_context_new_global (ctxt, /* gcc_jit_context *ctxt */
                                NULL, /* gcc_jit_location *loc */
                                GCC_JIT_GLOBAL_INTERNAL, /* enum gcc_jit_global_kind kind */
                                int_type, /* gcc_jit_type *type */
                                "idx"); /* const char *name */

  gcc_jit_block_add_assignment_op (block, /*gcc_jit_block *block */
                                   NULL, /* gcc_jit_location *loc */
                                   idx, /* gcc_jit_lvalue *lvalue */
                                   GCC_JIT_BINARY_OP_PLUS, /* enum gcc_jit_binary_op op */
                                   unsigned_char_1); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_void_return (block, /*gcc_jit_block *block */
				      NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_block_add_assignment_op:"
		      " mismatching types:"
		      " assignment to idx (type: int)"
		      " involving (unsigned char)1 (type: unsigned char)")
}
