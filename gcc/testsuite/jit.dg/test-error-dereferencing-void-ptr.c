#include <libgccjit.h>
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Replay of API calls for ctxt.  */
  gcc_jit_type *type_long_long =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG_LONG);
  gcc_jit_type *type_void =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *type_void_ptr =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_field *field_u_signed =
    gcc_jit_context_new_field (ctxt,
			       NULL, /* gcc_jit_location *loc */
			       type_long_long, /* gcc_jit_type *type, */
			       "u_signed"); /* const char *name */
  gcc_jit_field *field_u_ptr =
    gcc_jit_context_new_field (ctxt,
			       NULL, /* gcc_jit_location *loc */
			       type_void_ptr, /* gcc_jit_type *type, */
			       "u_ptr"); /* const char *name */
  gcc_jit_field *fields_for_union_any[2] = {
    field_u_signed,
    field_u_ptr,
  };
  gcc_jit_type *union_any =
    gcc_jit_context_new_union_type (ctxt,
				    NULL, /* gcc_jit_location *loc */
				    "any", /* const char *name */
				    2, /* int num_fields */
				    fields_for_union_any);
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, /* gcc_jit_context *ctxt */
				  NULL, /* gcc_jit_location *loc */
				  GCC_JIT_FUNCTION_EXPORTED,
				  type_void, /* gcc_jit_type *return_type */
				  "anonloop_0", /* const char *name */
				  0, /* int num_params */
				  NULL, /* gcc_jit_param **params */
				  0); /* int is_variadic */
  gcc_jit_block *block_initial =
    gcc_jit_function_new_block (func, "initial");

  gcc_jit_lvalue *local_tmp =
    gcc_jit_function_new_local (func, /* gcc_jit_function *func */
				NULL, /* gcc_jit_location *loc */
				union_any, /* gcc_jit_type *type */
				"tmp"); /* const char *name */

  /* "tmp.u_signed = 0x213d640;" */
  gcc_jit_block_add_assignment (
    block_initial, /*gcc_jit_block *block */
    NULL, /* gcc_jit_location *loc */
    gcc_jit_lvalue_access_field (local_tmp, /*gcc_jit_lvalue *struct_or_union */
				 NULL, /*gcc_jit_location *loc */
				 field_u_signed),
    gcc_jit_context_new_rvalue_from_long (
      ctxt, /* gcc_jit_context *ctxt */
      type_long_long, /* gcc_jit_type *numeric_type */
      0x213d640)); /* long value */

  /* "(*tmp.u_ptr) += 1;" which can't be done since u_ptr is a (void *).  */
  gcc_jit_block_add_assignment_op (
    block_initial, /*gcc_jit_block *block */
    NULL, /* gcc_jit_location *loc */
    /* "(*tmp.u_ptr)".  */
    gcc_jit_rvalue_dereference (
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_lvalue_access_field (
	  local_tmp, /*gcc_jit_lvalue *struct_or_union */
	  NULL, /*gcc_jit_location *loc */
	  field_u_ptr)),
      NULL), /* gcc_jit_location *loc */
    GCC_JIT_BINARY_OP_PLUS, /* enum gcc_jit_binary_op op */
    gcc_jit_context_new_rvalue_from_int (
      ctxt, /* gcc_jit_context *ctxt */
      type_long_long, /* gcc_jit_type *numeric_type */
      1)); /* int value */

  gcc_jit_block_end_with_void_return (block_initial, /*gcc_jit_block *block */
				      NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_VALUE (result, NULL);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_first_error (ctxt),
		      "gcc_jit_rvalue_dereference:"
		      " dereference of void pointer tmp.u_ptr"
		      " (type: void *)");
}
