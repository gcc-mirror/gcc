#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>
#include <stdbool.h>

#include "libgccjit.h"

#include "harness.h"

struct zoo
{
  void *m_void_ptr;

  bool m_bool;

  char m_char;
  signed char m_signed_char;
  unsigned char m_unsigned_char;

  short m_short;
  unsigned short m_unsigned_short;

  int m_int;
  unsigned int m_unsigned_int;

  long m_long;
  unsigned long m_unsigned_long;

  long long m_long_long;
  unsigned long long m_unsigned_long_long;

  uint8_t m_u8;
  uint16_t m_u16;
  uint32_t m_u32;
  uint64_t m_u64;

  int8_t m_i8;
  int16_t m_i16;
  int32_t m_i32;
  int64_t m_i64;

#ifdef __SIZEOF_INT128__
  __uint128_t m_u128;
  __int128_t m_i128;
#endif

  int m_sized_int_type;

  float m_float;
  double m_double;
  long double m_long_double;

  const char *m_const_char_ptr;

  size_t m_size_t;

  FILE *m_FILE_ptr;
};

int test_int = 42;
int *test_ptr = &test_int;

const char *test_string = "test_string";

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     void
     test_caller (struct zoo *z)
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

  gcc_jit_field *field_m_void_ptr =
    CREATE_FIELD (GCC_JIT_TYPE_VOID_PTR, "m_void_ptr");

  gcc_jit_field *field_m_bool =
    CREATE_FIELD (GCC_JIT_TYPE_BOOL, "m_bool");

  gcc_jit_field *field_m_char =
    CREATE_FIELD (GCC_JIT_TYPE_CHAR, "m_char");
  gcc_jit_field *field_m_signed_char =
    CREATE_FIELD (GCC_JIT_TYPE_SIGNED_CHAR, "m_signed_char");
  gcc_jit_field *field_m_unsigned_char =
    CREATE_FIELD (GCC_JIT_TYPE_UNSIGNED_CHAR, "m_unsigned_char");

  gcc_jit_field *field_m_short =
    CREATE_FIELD (GCC_JIT_TYPE_SHORT, "m_short");
  gcc_jit_field *field_m_unsigned_short =
    CREATE_FIELD (GCC_JIT_TYPE_UNSIGNED_SHORT, "m_unsigned_short");

  gcc_jit_field *field_m_int =
    CREATE_FIELD (GCC_JIT_TYPE_INT, "m_int");
  gcc_jit_field *field_m_unsigned_int =
    CREATE_FIELD (GCC_JIT_TYPE_UNSIGNED_INT, "m_unsigned_int");

  gcc_jit_field *field_m_long =
    CREATE_FIELD (GCC_JIT_TYPE_LONG, "m_long");
  gcc_jit_field *field_m_unsigned_long =
    CREATE_FIELD (GCC_JIT_TYPE_UNSIGNED_LONG, "m_unsigned_long");

  gcc_jit_field *field_m_long_long =
    CREATE_FIELD (GCC_JIT_TYPE_LONG_LONG, "m_long_long");
  gcc_jit_field *field_m_unsigned_long_long =
    CREATE_FIELD (GCC_JIT_TYPE_UNSIGNED_LONG_LONG, "m_unsigned_long_long");

  gcc_jit_field *field_m_u8 =
    CREATE_FIELD (GCC_JIT_TYPE_UINT8_T, "m_u8");
  gcc_jit_field *field_m_u16 =
    CREATE_FIELD (GCC_JIT_TYPE_UINT16_T, "m_u16");
  gcc_jit_field *field_m_u32 =
    CREATE_FIELD (GCC_JIT_TYPE_UINT32_T, "m_u32");
  gcc_jit_field *field_m_u64 =
    CREATE_FIELD (GCC_JIT_TYPE_UINT64_T, "m_u64");

  gcc_jit_field *field_m_i8 =
    CREATE_FIELD (GCC_JIT_TYPE_INT8_T, "m_i8");
  gcc_jit_field *field_m_i16 =
    CREATE_FIELD (GCC_JIT_TYPE_INT16_T, "m_i16");
  gcc_jit_field *field_m_i32 =
    CREATE_FIELD (GCC_JIT_TYPE_INT32_T, "m_i32");
  gcc_jit_field *field_m_i64 =
    CREATE_FIELD (GCC_JIT_TYPE_INT64_T, "m_i64");

#ifdef __SIZEOF_INT128__
  gcc_jit_field *field_m_u128 =
    CREATE_FIELD (GCC_JIT_TYPE_UINT128_T, "m_u128");
  gcc_jit_field *field_m_i128 =
    CREATE_FIELD (GCC_JIT_TYPE_INT128_T, "m_i128");
#endif

  /* Signed int type with sizeof (int): */
  gcc_jit_type *sized_int_type =
    gcc_jit_context_get_int_type (ctxt, sizeof (int), 1);
  gcc_jit_field *field_m_sized_int_type =
    gcc_jit_context_new_field (
      ctxt, NULL, sized_int_type, "m_sized_int_type");

  gcc_jit_field *field_m_float =
    CREATE_FIELD (GCC_JIT_TYPE_FLOAT, "m_float");
  gcc_jit_field *field_m_double =
    CREATE_FIELD (GCC_JIT_TYPE_DOUBLE, "m_double");
  gcc_jit_field *field_m_long_double =
    CREATE_FIELD (GCC_JIT_TYPE_LONG_DOUBLE, "m_long_double");

  gcc_jit_field *field_m_const_char_ptr =
    CREATE_FIELD (GCC_JIT_TYPE_CONST_CHAR_PTR, "m_const_char_ptr");

  gcc_jit_field *field_m_size_t =
    CREATE_FIELD (GCC_JIT_TYPE_SIZE_T, "m_size_t");

  gcc_jit_field *field_m_FILE_ptr =
    CREATE_FIELD (GCC_JIT_TYPE_FILE_PTR, "m_FILE_ptr");

#undef CREATE_FIELD

  gcc_jit_field *zoo_fields[] = {
    field_m_void_ptr,

    field_m_bool,

    field_m_char,
    field_m_signed_char,
    field_m_unsigned_char,

    field_m_short,
    field_m_unsigned_short,

    field_m_int,
    field_m_unsigned_int,

    field_m_long,
    field_m_unsigned_long,

    field_m_long_long,
    field_m_unsigned_long_long,

    field_m_u8,
    field_m_u16,
    field_m_u32,
    field_m_u64,

    field_m_i8,
    field_m_i16,
    field_m_i32,
    field_m_i64,

#ifdef __SIZEOF_INT128__
    field_m_u128,
    field_m_i128,
#endif

    field_m_sized_int_type,

    field_m_float,
    field_m_double,
    field_m_long_double,

    field_m_const_char_ptr,

    field_m_size_t,

    field_m_FILE_ptr
  };

  gcc_jit_type *zoo_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (
        ctxt,
	NULL,
	"zoo",
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
				  "test_types",
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

  ASSIGN(
    field_m_void_ptr,
    gcc_jit_context_new_rvalue_from_ptr (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR),
      test_ptr))

  ASSIGN(field_m_bool,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_BOOL), 1))

  ASSIGN(field_m_char,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR),
      'V'))
  ASSIGN(field_m_signed_char,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SIGNED_CHAR),
      -37))
  ASSIGN(field_m_unsigned_char,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_CHAR),
      200))

  ASSIGN(field_m_short,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SHORT),
      -900))
  ASSIGN(field_m_unsigned_short,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_SHORT),
      0x3000))

  ASSIGN(field_m_int,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT),
      -0x2000))
  ASSIGN(field_m_unsigned_int,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_INT),
      1234567))

  ASSIGN(field_m_long,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG),
      -5))
  ASSIGN(field_m_unsigned_long,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_LONG),
      12345678))

  ASSIGN(field_m_long_long,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG_LONG),
      -42))
  ASSIGN(field_m_unsigned_long_long,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_LONG_LONG),
      123456789))

  ASSIGN(field_m_u8,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UINT8_T),
      123))
  ASSIGN(field_m_u16,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UINT16_T),
      12345))
  ASSIGN(field_m_u32,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UINT32_T),
      123456789))
  ASSIGN(field_m_u64,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UINT64_T),
      123456789))

  ASSIGN(field_m_i8,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT8_T),
      -1))
  ASSIGN(field_m_i16,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT16_T),
      -2))
  ASSIGN(field_m_i32,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT32_T),
      -3))
  ASSIGN(field_m_i64,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT64_T),
      -4))

#ifdef __SIZEOF_INT128__
  ASSIGN(field_m_u128,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UINT128_T),
      123456789))
  ASSIGN(field_m_i128,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT128_T),
      -5))
#endif

  ASSIGN(field_m_sized_int_type,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      sized_int_type, 500))

  ASSIGN(field_m_float,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT),
      3.141))
  ASSIGN(field_m_double,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE),
      3.141))
  ASSIGN(field_m_long_double,
    gcc_jit_context_new_rvalue_from_double (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG_DOUBLE),
      3.141))

  ASSIGN(field_m_const_char_ptr,
    gcc_jit_context_new_rvalue_from_ptr (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR),
      (char *)test_string))

  ASSIGN(field_m_size_t,
    gcc_jit_context_new_rvalue_from_int (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SIZE_T),
      sizeof (struct zoo)))

  ASSIGN(field_m_FILE_ptr,
    gcc_jit_context_new_rvalue_from_ptr (
      ctxt,
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FILE_PTR),
      stderr))

#undef ASSIGN

  gcc_jit_block_end_with_void_return (block, NULL);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef void (*fn_type) (struct zoo *);
  CHECK_NON_NULL (result);

  fn_type test_types =
    (fn_type)gcc_jit_result_get_code (result, "test_types");
  CHECK_NON_NULL (test_types);

  struct zoo z;
  memset (&z, 0xf0, sizeof (z));

  /* Call the JIT-generated function.  */
  test_types (&z);

  /* Verify that it correctly wrote to the various fields.  */
  CHECK_VALUE (z.m_void_ptr, test_ptr);

  CHECK_VALUE (z.m_bool, true);

  CHECK_VALUE (z.m_char, 'V');
  CHECK_VALUE (z.m_signed_char, -37);
  CHECK_VALUE (z.m_unsigned_char, 200);

  CHECK_VALUE (z.m_short, -900);
  CHECK_VALUE (z.m_unsigned_short, 0x3000);

  CHECK_VALUE (z.m_int, -0x2000);
  CHECK_VALUE (z.m_unsigned_int, 1234567);

  CHECK_VALUE (z.m_long, -5);
  CHECK_VALUE (z.m_unsigned_long, 12345678);

  CHECK_VALUE (z.m_long_long, -42);
  CHECK_VALUE (z.m_unsigned_long_long, 123456789);

  CHECK_VALUE (z.m_u8, 123);
  CHECK_VALUE (z.m_u16, 12345);
  CHECK_VALUE (z.m_u32, 123456789);
  CHECK_VALUE (z.m_u64, 123456789);

  CHECK_VALUE (z.m_i8, -1);
  CHECK_VALUE (z.m_i16, -2);
  CHECK_VALUE (z.m_i32, -3);
  CHECK_VALUE (z.m_i64, -4);

#ifdef __SIZEOF_INT128__
  CHECK_VALUE (z.m_u128, 123456789);
  CHECK_VALUE (z.m_i128, -5);
#endif

  CHECK_VALUE (z.m_sized_int_type, 500);

  CHECK_VALUE (z.m_float, 3.141f);
  CHECK_VALUE (z.m_double, 3.141);
  CHECK_VALUE (z.m_long_double, 3.141);

  CHECK_VALUE (z.m_const_char_ptr, test_string);

  CHECK_VALUE (z.m_size_t, sizeof (struct zoo));

  CHECK_VALUE (z.m_FILE_ptr, stderr);

  if (sizeof(long) == 8)
    CHECK (gcc_jit_compatible_types (
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG),
      gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT64_T)));

  CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_FLOAT)), sizeof (float));
  CHECK_VALUE (gcc_jit_type_get_size (gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE)), sizeof (double));
}
