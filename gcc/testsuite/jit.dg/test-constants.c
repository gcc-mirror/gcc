#include <limits.h>
#include <float.h>

#include "libgccjit.h"

#include "harness.h"

static void
make_test_of_constant (gcc_jit_context *ctxt,
                       gcc_jit_type *type,
                       gcc_jit_rvalue *rvalue,
                       const char *funcname)
{
  /* Make a test function of the form:
       T funcname (void)
       {
	  return VALUE;
       }
     and return a debug dump of VALUE so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_function *test_fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  type,
				  funcname,
				  0, NULL,
				  0);
  gcc_jit_block *initial = gcc_jit_function_new_block (test_fn, "initial");
  gcc_jit_block_end_with_return (initial, NULL, rvalue);
}

/**********************************************************************
 Tests of gcc_jit_context_new_rvalue_from_int.
 **********************************************************************/

static const char *
make_test_of_int_constant (gcc_jit_context *ctxt,
			   gcc_jit_type *type,
			   int value,
			   const char *funcname)
{
  /* Make a test function of the form:
       int funcname (void)
       {
	  return VALUE;
       }
     and return a debug dump of VALUE so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_rvalue *rvalue =
    gcc_jit_context_new_rvalue_from_int (ctxt, type, value);
  make_test_of_constant (ctxt, type, rvalue, funcname);
  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (rvalue));
}

static void
make_tests_of_int_constants (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);

  CHECK_STRING_VALUE (
    make_test_of_int_constant (ctxt,
			       int_type,
			       0,
			       "test_int_constant_0"),
    "(int)0");
  make_test_of_int_constant (ctxt,
                             int_type,
                             INT_MAX,
                             "test_int_constant_INT_MAX");
  make_test_of_int_constant (ctxt,
                             int_type,
                             INT_MIN,
                             "test_int_constant_INT_MIN");
}

static void
verify_int_constants (gcc_jit_result *result)
{
  typedef int (*test_fn) (void);

  test_fn test_int_constant_0 =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_int_constant_0");
  CHECK_NON_NULL (test_int_constant_0);
  CHECK_VALUE (test_int_constant_0 (), 0);

  test_fn test_int_constant_INT_MAX =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_int_constant_INT_MAX");
  CHECK_NON_NULL (test_int_constant_INT_MAX);
  CHECK_VALUE (test_int_constant_INT_MAX (), INT_MAX);

  test_fn test_int_constant_INT_MIN =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_int_constant_INT_MIN");
  CHECK_NON_NULL (test_int_constant_INT_MIN);
  CHECK_VALUE (test_int_constant_INT_MIN (), INT_MIN);
}

/**********************************************************************
 Tests of gcc_jit_context_new_rvalue_from_long.
 **********************************************************************/

static const char *
make_test_of_long_constant (gcc_jit_context *ctxt,
			   gcc_jit_type *type,
			   long value,
			   const char *funcname)
{
  /* Make a test function of the form:
       long funcname (void)
       {
	  return VALUE;
       }
     and return a debug dump of VALUE so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_rvalue *rvalue =
    gcc_jit_context_new_rvalue_from_long (ctxt, type, value);
  make_test_of_constant (ctxt, type, rvalue, funcname);
  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (rvalue));
}

static void
make_tests_of_long_constants (gcc_jit_context *ctxt)
{
  gcc_jit_type *long_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_LONG);

  CHECK_STRING_VALUE (
    make_test_of_long_constant (ctxt,
			       long_type,
			       0,
			       "test_long_constant_0"),
    "(long)0");
  make_test_of_long_constant (ctxt,
                             long_type,
                             LONG_MAX,
                             "test_long_constant_LONG_MAX");
  make_test_of_long_constant (ctxt,
                             long_type,
                             LONG_MIN,
                             "test_long_constant_LONG_MIN");
}

static void
verify_long_constants (gcc_jit_result *result)
{
  typedef long (*test_fn) (void);

  test_fn test_long_constant_0 =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_long_constant_0");
  CHECK_NON_NULL (test_long_constant_0);
  CHECK_VALUE (test_long_constant_0 (), 0);

  test_fn test_long_constant_LONG_MAX =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_long_constant_LONG_MAX");
  CHECK_NON_NULL (test_long_constant_LONG_MAX);
  CHECK_VALUE (test_long_constant_LONG_MAX (), LONG_MAX);

  test_fn test_long_constant_LONG_MIN =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_long_constant_LONG_MIN");
  CHECK_NON_NULL (test_long_constant_LONG_MIN);
  CHECK_VALUE (test_long_constant_LONG_MIN (), LONG_MIN);
}

/**********************************************************************
 Tests of gcc_jit_context_new_rvalue_from_double.
 **********************************************************************/

static const char *
make_test_of_double_constant (gcc_jit_context *ctxt,
			   gcc_jit_type *type,
			   double value,
			   const char *funcname)
{
  /* Make a test function of the form:
       double funcname (void)
       {
	  return VALUE;
       }
     and return a debug dump of VALUE so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_rvalue *rvalue =
    gcc_jit_context_new_rvalue_from_double (ctxt, type, value);
  make_test_of_constant (ctxt, type, rvalue, funcname);
  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (rvalue));
}

static void
make_tests_of_double_constants (gcc_jit_context *ctxt)
{
  gcc_jit_type *double_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_DOUBLE);

  make_test_of_double_constant (ctxt,
                                double_type,
                                0.5,
                                "test_double_constant_0_5");
  make_test_of_double_constant (ctxt,
                                double_type,
                                1e100,
                                "test_double_constant_1e100");
  make_test_of_double_constant (ctxt,
                                double_type,
                                DBL_MIN,
                                "test_double_constant_DBL_MIN");
  make_test_of_double_constant (ctxt,
                                double_type,
                                DBL_MAX,
                                "test_double_constant_DBL_MAX");
}

static void
verify_double_constants (gcc_jit_result *result)
{
  typedef double (*test_fn) (void);

  test_fn test_double_constant_0_5 =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_double_constant_0_5");
  CHECK_NON_NULL (test_double_constant_0_5);
  CHECK_VALUE (test_double_constant_0_5 (), 0.5);

  test_fn test_double_constant_1e100 =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_double_constant_1e100");
  CHECK_NON_NULL (test_double_constant_1e100);
  CHECK_VALUE (test_double_constant_1e100 (), 1e100);

  test_fn test_double_constant_DBL_MIN =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_double_constant_DBL_MIN");
  CHECK_NON_NULL (test_double_constant_DBL_MIN);
  CHECK_VALUE (test_double_constant_DBL_MIN (), DBL_MIN);

  test_fn test_double_constant_DBL_MAX =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_double_constant_DBL_MAX");
  CHECK_NON_NULL (test_double_constant_DBL_MAX);
  CHECK_VALUE (test_double_constant_DBL_MAX (), DBL_MAX);
}

/**********************************************************************
 Tests of gcc_jit_context_new_rvalue_from_ptr.
 **********************************************************************/

static const char *
make_test_of_ptr_constant (gcc_jit_context *ctxt,
			   gcc_jit_type *type,
			   void *value,
			   const char *funcname)
{
  /* Make a test function of the form:
       void *funcname (void)
       {
	  return VALUE;
       }
     and return a debug dump of VALUE so that
     the caller can sanity-check the debug dump implementation.
  */
  gcc_jit_rvalue *rvalue =
    gcc_jit_context_new_rvalue_from_ptr (ctxt, type, value);
  make_test_of_constant (ctxt, type, rvalue, funcname);
  return gcc_jit_object_get_debug_string (
    gcc_jit_rvalue_as_object (rvalue));
}

static void
make_tests_of_ptr_constants (gcc_jit_context *ctxt)
{
  gcc_jit_type *ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID_PTR);

  CHECK_STRING_VALUE (
    make_test_of_ptr_constant (ctxt,
			       ptr_type,
			       0,
			       "test_ptr_constant_0"),
    "(void *)NULL");
  CHECK_STRING_VALUE (
    make_test_of_ptr_constant (ctxt,
			       ptr_type,
			       (void *)0xdeadbeef,
			       "test_ptr_constant_0xdeadbeef"),
    "(void *)0xdeadbeef");
}

static void
verify_ptr_constants (gcc_jit_result *result)
{
  typedef void *(*test_fn) (void);

  test_fn test_ptr_constant_0 =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_ptr_constant_0");
  CHECK_NON_NULL (test_ptr_constant_0);
  CHECK_VALUE (test_ptr_constant_0 (), 0);

  test_fn test_ptr_constant_0xdeadbeef =
    (test_fn)gcc_jit_result_get_code (result,
				      "test_ptr_constant_0xdeadbeef");
  CHECK_NON_NULL (test_ptr_constant_0xdeadbeef);
  CHECK_VALUE (test_ptr_constant_0xdeadbeef (), (void *)0xdeadbeef);
}

/**********************************************************************
 Code for harness
 **********************************************************************/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  make_tests_of_int_constants (ctxt);
  make_tests_of_long_constants (ctxt);
  make_tests_of_double_constants (ctxt);
  make_tests_of_ptr_constants (ctxt);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  verify_int_constants (result);
  verify_long_constants (result);
  verify_double_constants (result);
  verify_ptr_constants (result);
}
