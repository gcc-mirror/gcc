#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#include "libgccjit.h"

#include "harness.h"

struct float_zoo
{
  _Float16 m_float16;
  _Float32 m_float32;
  _Float64 m_float64;
  __float128 m_float128;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_caller (struct float_zoo *z)
     {
	for each fields "m_field":
	  z->m_field = ...some data;
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

#define CREATE_FIELD(TYPE, NAME) \
  gcc_jit_context_new_field ( \
	ctxt, NULL, \
	gcc_jit_context_get_type (ctxt, TYPE), \
	NAME)

  gcc_jit_context *info_ctxt = gcc_jit_context_acquire ();
  gcc_jit_target_info *target_info = gcc_jit_context_get_target_info (info_ctxt);

  enum gcc_jit_types float_type1 = GCC_JIT_TYPE_FLOAT;
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT16))
    float_type1 = GCC_JIT_TYPE_FLOAT16;

  enum gcc_jit_types float_type2 = GCC_JIT_TYPE_FLOAT;
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT32))
    float_type2 = GCC_JIT_TYPE_FLOAT32;

  enum gcc_jit_types float_type3 = GCC_JIT_TYPE_FLOAT;
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT64))
    float_type3 = GCC_JIT_TYPE_FLOAT64;

  enum gcc_jit_types float_type4 = GCC_JIT_TYPE_FLOAT;
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT128))
    float_type4 = GCC_JIT_TYPE_FLOAT128;

  gcc_jit_field *field_m_float16 = CREATE_FIELD(float_type1, "m_float16");
  gcc_jit_field *field_m_float32 = CREATE_FIELD(float_type2, "m_float32");
  gcc_jit_field *field_m_float64 = CREATE_FIELD(float_type3, "m_float64");
  gcc_jit_field *field_m_float128 = CREATE_FIELD(float_type4, "m_float128");

#undef CREATE_FIELD

  gcc_jit_field *zoo_fields[] = {
    field_m_float16,
    field_m_float32,
    field_m_float64,
    field_m_float128,
  };

  gcc_jit_type *zoo_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (
        ctxt,
	NULL,
	"float_zoo",
	sizeof (zoo_fields) / sizeof (zoo_fields[0]),
	zoo_fields));

  gcc_jit_type *zoo_ptr_type =
    gcc_jit_type_get_pointer (zoo_type);

  /* Build the test_fn.	 */
  gcc_jit_param *param_z =
    gcc_jit_context_new_param (ctxt, NULL, zoo_ptr_type, "z");
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  void_type,
				  "test_float_types",
				  1, &param_z,
				  0);
  gcc_jit_block *block = gcc_jit_function_new_block (test_fn, NULL);

  /* Write to the various fields of param "z".	*/
#define ASSIGN(FIELD, EXPR) \
  gcc_jit_block_add_assignment (		\
    block, NULL,				\
    gcc_jit_rvalue_dereference_field (		\
      gcc_jit_param_as_rvalue (param_z),	\
      NULL,					\
      (FIELD)),				\
    (EXPR));

  ASSIGN(field_m_float16,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, float_type1),
      3.141))
  ASSIGN(field_m_float32,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, float_type2),
      3.141))
  ASSIGN(field_m_float64,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, float_type3),
      3.141))
  ASSIGN(field_m_float128,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, float_type4),
      3.141))

#undef ASSIGN

  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (struct float_zoo *);
  CHECK_NON_NULL (result);

  fn_type test_float_types =
    (fn_type)gcc_jit_result_get_code (result, "test_float_types");
  CHECK_NON_NULL (test_float_types);

  struct float_zoo z;
  memset (&z, 0xf0, sizeof (z));

  /* Call the JIT-generated function.  */
  test_float_types (&z);

  /* Verify that it correctly wrote to the various fields.  */
  gcc_jit_context *info_ctxt = gcc_jit_context_acquire ();
  gcc_jit_target_info *target_info = gcc_jit_context_get_target_info (info_ctxt);
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT16))
    CHECK_VALUE (z.m_float16, (_Float16)3.141);
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT32))
    CHECK_VALUE (z.m_float32, (_Float32)3.141);
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT64))
    CHECK_VALUE (z.m_float64, (_Float64)3.141);
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT128))
    CHECK_VALUE (z.m_float128, (__float128)3.141);

  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT16))
    CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT16)), sizeof (_Float16));
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT32))
    CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT32)), sizeof (_Float32));
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT64))
    CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT64)), sizeof (_Float64));
  if (gcc_jit_target_info_supports_target_dependent_type (target_info, GCC_JIT_TYPE_FLOAT128))
    CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT128)), sizeof (__float128));
}
