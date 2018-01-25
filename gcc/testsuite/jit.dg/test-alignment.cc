#include <stdlib.h>
#include <stdio.h>

#include "libgccjit++.h"

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
create_aligned_code (gcc_jit_context *c_ctxt, const char *struct_name,
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
  gccjit::context ctxt (c_ctxt);
  gccjit::type char_type = ctxt.get_type (GCC_JIT_TYPE_CHAR);
  gccjit::type aligned_char_type = char_type.get_aligned (alignment);
  gccjit::field x = ctxt.new_field (aligned_char_type, "x");
  gccjit::field y = ctxt.new_field (aligned_char_type, "y");
  std::vector<gccjit::field> fields = {x, y};
  gccjit::type struct_type = ctxt.new_struct_type (struct_name, fields);
  gccjit::type const_struct_type = struct_type.get_const ();
  gccjit::type const_ptr_type = const_struct_type.get_pointer ();

  /* Build the reader fn.  */
  gccjit::param param_f = ctxt.new_param (const_ptr_type, "f");
  std::vector<gccjit::param> params = {param_f};
  gccjit::function fn_test_reading
    = ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                         char_type,
                         reader_fn_name,
                         params,
                         0);

  /* return f->x * f->y; */
  gccjit::block reading_block = fn_test_reading.new_block ();
  reading_block.end_with_return (param_f.dereference_field (x)
                                 * param_f.dereference_field (y));

  /* Build the writer fn.  */
  gccjit::type ptr_type = struct_type.get_pointer ();
  gccjit::param param_g = ctxt.new_param (ptr_type, "g");
  params = {param_g};
  gccjit::function fn_test_writing
    = ctxt.new_function (GCC_JIT_FUNCTION_EXPORTED,
                         char_type,
                         writer_fn_name,
                         params,
                         0);

  /* g->x = 5; */
  gccjit::block writing_block = fn_test_writing.new_block ();
  writing_block.add_assignment (param_g.dereference_field (x),
                                ctxt.new_rvalue (char_type, 5));

  /* g->y = 7; */
  writing_block.add_assignment (param_g.dereference_field (y),
                                ctxt.new_rvalue (char_type, 7));

  /* return READER_FN_NAME (g); */
  writing_block.end_with_return (ctxt.new_call (fn_test_reading,
                                                param_g));
}

/* Implement a verifier function for a given struct.  */

template <typename T>
static void
verify_aligned_code (gcc_jit_context *ctxt,
                     gcc_jit_result *result,
                     const char *writer_fn_name)
{
  typedef char (*fn_type) (T *);
  CHECK_NON_NULL (result);

  T tmp;
  memset (&tmp, 0xac, sizeof (tmp));
  fn_type test_writing =
    (fn_type)gcc_jit_result_get_code (result, writer_fn_name);
  CHECK_NON_NULL (test_writing);

  /* Verify that the code correctly returns the product of the fields.  */
  CHECK_VALUE (test_writing (&tmp), 35);

  /* Verify the we can read the values of the fields, and thus that the
     struct layout agrees with that of the C++ frontend.  */
  CHECK_VALUE (tmp.x, 5);
  CHECK_VALUE (tmp.y, 7);
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

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  verify_aligned_code<s2> (ctxt, result, "test_aligned_writing_s2");
  verify_aligned_code<s4> (ctxt, result, "test_aligned_writing_s4");
  verify_aligned_code<s8> (ctxt, result, "test_aligned_writing_s8");
  verify_aligned_code<s16> (ctxt, result, "test_aligned_writing_s16");
  verify_aligned_code<s32> (ctxt, result, "test_aligned_writing_s32");
  verify_aligned_code<s64> (ctxt, result, "test_aligned_writing_s64");
  verify_aligned_code<s128> (ctxt, result, "test_aligned_writing_s128");
}
