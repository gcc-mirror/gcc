#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

struct s2
{
  char x __attribute__ ((aligned (2)));
  char y __attribute__ ((aligned (2)));
};

struct s4
{
  char x __attribute__ ((aligned (4)));
  char y __attribute__ ((aligned (4)));
};

struct s8
{
  char x __attribute__ ((aligned (8)));
  char y __attribute__ ((aligned (8)));
};

struct s16
{
  char x __attribute__ ((aligned (16)));
  char y __attribute__ ((aligned (16)));
};

struct s32
{
  char x __attribute__ ((aligned (32)));
  char y __attribute__ ((aligned (32)));
};

struct s64
{
  char x __attribute__ ((aligned (64)));
  char y __attribute__ ((aligned (64)));
};

struct s128
{
  char x __attribute__ ((aligned (128)));
  char y __attribute__ ((aligned (128)));
};

static void
create_aligned_code (gcc_jit_context *ctxt, const char *struct_name,
		     unsigned int alignment, const char *reader_fn_name,
		     const char *writer_fn_name)
{
  /* Let's try to inject the equivalent of:

     char
     READER_FN_NAME (const struct STRUCT_NAME *f)
     {
       return f->x * f->y;
     }

     char
     WRITER_FN_NAME (struct STRUCT_NAME *g)
     {
       g->x = 5;
       g->y = 7;
       return READER_FN_NAME (g);
     }
  */
  gcc_jit_type *char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR);
  gcc_jit_type *aligned_char_type =
    gcc_jit_type_get_aligned (char_type, alignment);
  gcc_jit_field *x =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               aligned_char_type,
                               "x");
  gcc_jit_field *y =
    gcc_jit_context_new_field (ctxt,
                               NULL,
                               aligned_char_type,
                               "y");
  gcc_jit_field *fields[] = {x, y};
  gcc_jit_type *struct_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt, NULL, struct_name, 2, fields));
  gcc_jit_type *const_struct_type = gcc_jit_type_get_const (struct_type);
  gcc_jit_type *const_ptr_type = gcc_jit_type_get_pointer (const_struct_type);

  /* Build the reader fn.  */
  gcc_jit_param *param_f =
    gcc_jit_context_new_param (ctxt, NULL, const_ptr_type, "f");
  gcc_jit_function *fn_test_reading =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  char_type,
				  reader_fn_name,
                                  1, &param_f,
                                  0);

  /* return f->x * f->y; */
  gcc_jit_block *reading_block = gcc_jit_function_new_block (fn_test_reading, NULL);
  gcc_jit_block_end_with_return (
    reading_block,
    NULL,
    gcc_jit_context_new_binary_op (
      ctxt, NULL,
      GCC_JIT_BINARY_OP_MULT,
      char_type,
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	  gcc_jit_param_as_rvalue (param_f),
	  NULL,
	  x)),
      gcc_jit_lvalue_as_rvalue (
	gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_f),
	NULL,
	y))));

  /* Build the writer fn.  */
  gcc_jit_type *ptr_type = gcc_jit_type_get_pointer (struct_type);
  gcc_jit_param *param_g =
    gcc_jit_context_new_param (ctxt, NULL, ptr_type, "g");
  gcc_jit_function *fn_test_writing =
    gcc_jit_context_new_function (ctxt, NULL,
                                  GCC_JIT_FUNCTION_EXPORTED,
                                  char_type,
                                  writer_fn_name,
                                  1, &param_g,
                                  0);

  /* g->x = 5; */
  gcc_jit_block *writing_block = gcc_jit_function_new_block (fn_test_writing, NULL);
  gcc_jit_block_add_assignment (
    writing_block, NULL,
    gcc_jit_rvalue_dereference_field (gcc_jit_param_as_rvalue (param_g),
				      NULL, x),
    gcc_jit_context_new_rvalue_from_int (ctxt, char_type, 5));

  /* g->y = 7; */
  gcc_jit_block_add_assignment (
    writing_block, NULL,
    gcc_jit_rvalue_dereference_field (gcc_jit_param_as_rvalue (param_g),
				      NULL, y),
    gcc_jit_context_new_rvalue_from_int (ctxt, char_type, 7));

  /* return READER_FN_NAME (g); */
  gcc_jit_rvalue *arg = gcc_jit_param_as_rvalue (param_g);
  gcc_jit_block_end_with_return (
    writing_block,
    NULL,
    gcc_jit_context_new_call (
      ctxt, NULL,
      fn_test_reading,
      1, &arg));
}

/* Implement a verifier function for a given struct.  */

#define IMPL_VERIFY_ALIGNED_CODE(TYPENAME) \
  static void								\
  verify_aligned_code_ ##TYPENAME (gcc_jit_context *ctxt,		\
				   gcc_jit_result *result,		\
				   const char *writer_fn_name)		\
  {									\
  typedef char (*fn_type) (struct TYPENAME *);				\
  CHECK_NON_NULL (result);						\
									\
  struct TYPENAME tmp;							\
  memset (&tmp, 0xac, sizeof (tmp));					\
									\
  fn_type test_writing =						\
    (fn_type)gcc_jit_result_get_code (result, writer_fn_name);		\
  CHECK_NON_NULL (test_writing);					\
									\
  /* Verify that the code correctly returns the product of the fields.  */ \
  CHECK_VALUE (test_writing (&tmp), 35);				\
									\
  /* Verify the we can read the values of the fields, and thus that the \
     struct layout agrees with that of the C frontend.  */		\
  CHECK_VALUE (tmp.x, 5);						\
  CHECK_VALUE (tmp.y, 7);						\
  }

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  create_aligned_code (ctxt, "s2", 2, "test_aligned_reading_s2",
		       "test_aligned_writing_s2");
  create_aligned_code (ctxt, "s4", 4, "test_aligned_reading_s4",
		       "test_aligned_writing_s4");
  create_aligned_code (ctxt, "s8", 8, "test_aligned_reading_s8",
		       "test_aligned_writing_s8");
  create_aligned_code (ctxt, "s16", 16, "test_aligned_reading_s16",
		       "test_aligned_writing_s16");
  create_aligned_code (ctxt, "s32", 32, "test_aligned_reading_s32",
		       "test_aligned_writing_s32");
  create_aligned_code (ctxt, "s64", 64, "test_aligned_reading_s64",
		       "test_aligned_writing_s64");
  create_aligned_code (ctxt, "s128", 128, "test_aligned_reading_s128",
		       "test_aligned_writing_s128");
}

IMPL_VERIFY_ALIGNED_CODE(s2)
IMPL_VERIFY_ALIGNED_CODE(s4)
IMPL_VERIFY_ALIGNED_CODE(s8)
IMPL_VERIFY_ALIGNED_CODE(s16)
IMPL_VERIFY_ALIGNED_CODE(s32)
IMPL_VERIFY_ALIGNED_CODE(s64)
IMPL_VERIFY_ALIGNED_CODE(s128)

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_aligned_code_s2 (ctxt, result,
			  "test_aligned_writing_s2");
  verify_aligned_code_s4 (ctxt, result,
			  "test_aligned_writing_s4");
  verify_aligned_code_s8 (ctxt, result,
			  "test_aligned_writing_s8");
  verify_aligned_code_s16 (ctxt, result,
			   "test_aligned_writing_s16");
  verify_aligned_code_s32 (ctxt, result,
			   "test_aligned_writing_s32");
  verify_aligned_code_s64 (ctxt, result,
			   "test_aligned_writing_s64");
  verify_aligned_code_s128 (ctxt, result,
			   "test_aligned_writing_s128");
}
