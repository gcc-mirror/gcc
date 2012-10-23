/* This header file should be included for the purpose of parameter passing
   testing and va_arg code gen testing.

   To test va_arg code gen, #define AAPCS64_TEST_STDARG in the test case.

   The parameter passing test is done by passing variables/constants to
   'myfunc', which pushes its incoming arguments to a memory block on the
   stack and then passes the memory block address to 'testfunc'.  It is inside
   'testfunc' that the real parameter passing check is carried out.

   The function body of 'myfunc' is in abitest.S.  The declaration of 'myfunc'
   is constructed during the pre-processing stage.

   The va_arg code gen test has a similar workflow, apart from an extra set-up
   step before calling 'myfunc'.  All arguments are passed to 'stdarg_func'
   first, which assigned these arguments to its local variables via either
   direct assignment or va_arg macro, depending on whether an argument is named
   or not.  Afterwards, 'stdarg_func' calls 'myfunc' with the aforementioned
   local variables as the arguments to finish the remaining steps.  */

#include "abitest-common.h"
#include "validate_memory.h"

#ifdef AAPCS64_TEST_STDARG
/* Generate va_start (ap, last_named_arg).  Note that this requires
   LAST_NAMED_ARG_ID to be defined/used correctly in the test file.  */
#ifndef LAST_NAMED_ARG_ID
#define LAST_NAMED_ARG_ID 65535
#endif
#ifndef VA_START
#undef VA_START_1
#define VA_START_1(ap, id) va_start (ap, _f##id);
#define VA_START(ap, id) VA_START_1 (ap, id);
#endif
#endif /* AAPCS64_TEST_STDARG */

/* Some debugging facility.  */
#undef DUMP_ARG
#ifdef DUMP_ENABLED
#define DUMP_ARG(type,val) printf ("### Checking ARG "#type" "#val"\n")
#else
#define DUMP_ARG(type,val)
#endif


/* Function called from myfunc (defined in abitest.S) to check the arguments
   passed to myfunc.  myfunc has pushed all the arguments into the memory
   block pointed by STACK.  */
void testfunc(char* stack)
{
#define AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_MACRO_DEF_CHECK_INCOMING_ARGS
  return;
}


#ifndef AAPCS64_TEST_STDARG
/* Test parameter passing.  */

/* Function declaration of myfunc.  */
MYFUNCTYPE myfunc(
#define AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_MACRO_DEF_GEN_PARAM_TYPE_LIST
) PCSATTR;

#else /* AAPCS64_TEST_STDARG */
/* Test stdarg macros, e.g. va_arg.  */
#include <stdarg.h>

/* Dummy function to help reset parameter passing registers, i.e. X0-X7
   and V0-V7 (by being passed 0 in W0-W7 and 0.f in S0-S7).  */
__attribute__ ((noinline)) void
dummy_func (int w0, int w1, int w2, int w3, int w4, int w5, int w6, int w7,
	    float s0, float s1, float s2, float s3, float s4, float s5,
	    float s6, float s7)
{
  asm (""); /* Prevent function from getting optimized away */
  return;
}

/* Function declaration of myfunc.  */
MYFUNCTYPE myfunc(
#define AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST
) PCSATTR;

/* Function definition of stdarg_func.
   stdarg_func is a variadic function; it retrieves all of its arguments,
   both named and unnamed, and passes them to myfunc in the identical
   order.  myfunc will carry out the check on the passed values.  Remember
   that myfunc is not a variadic function.  */
MYFUNCTYPE stdarg_func(
#define AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_VARIADIC_MACRO_DEF_GEN_PARAM_TYPE_LIST_WITH_IDENT
) PCSATTR
{
  /* Start of the function body of stdarg_func.  */
  va_list ap;

  VA_START (ap, LAST_NAMED_ARG_ID)
  /* Zeroize the content of X0-X7 and V0-V7 to make sure that any va_arg
     failure will not be hidden by the old data being in these registers.  */
  dummy_func (0, 0, 0, 0, 0, 0, 0, 0, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f, 0.f);
  /* A full memory barrier to ensure that compiler won't optimize away
     va_arg code gen.  */
  __sync_synchronize ();
    {
      /* Assign all the function incoming arguments to local variables. */
#define AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_VARIADIC_MACRO_DEF_ASSIGN_LOCAL_VARS_WITH_ARGS

      /* Call myfunc and pass in the local variables prepared above.  */
      myfunc (
#define AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_VARIADIC_MACRO_DEF_GEN_ARGUMENT_LIST
);
    }
  va_end (ap);
}

#endif /* AAPCS64_TEST_STDARG */


int main()
{
#ifdef RUNTIME_ENDIANNESS_CHECK
  rt_endian_check();
#endif
#ifdef HAS_DATA_INIT_FUNC
  init_data ();
#endif

#ifndef AAPCS64_TEST_STDARG
  which_kind_of_test = TK_PARAM;
  myfunc(
#else
  which_kind_of_test = TK_VA_ARG;
  stdarg_func(
#endif
#define AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST
#include "macro-def.h"
#include TESTFILE
#undef AARCH64_MACRO_DEF_GEN_ARGUMENT_LIST
);
  return 0;
}

