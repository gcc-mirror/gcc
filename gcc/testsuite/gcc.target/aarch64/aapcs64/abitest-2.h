/* This header file should be included for the purpose of function return
   value testing.  */

#include "abitest-common.h"
#include "validate_memory.h"

void (*testfunc_ptr)(char* stack);
unsigned long long saved_return_address;

/* Helper macros to generate function name.  Example of the function name:
   func_return_val_1.  */
#define FUNC_BASE_NAME func_return_val_
#define FUNC_NAME_COMBINE(base,suffix) base ## suffix
#define FUNC_NAME_1(base,suffix) FUNC_NAME_COMBINE(base,suffix)
#define FUNC_NAME(suffix) FUNC_NAME_1(FUNC_BASE_NAME,suffix)
#define TEST_FUNC_BASE_NAME testfunc_
#define TEST_FUNC_NAME(suffix) FUNC_NAME_1(TEST_FUNC_BASE_NAME,suffix)

#undef DUMP_STATUS
#ifdef DUMP_ENABLED
#define DUMP_STATUS(type,val) printf ("### Checking "#type" "#val"\n");
#else
#define DUMP_STATUS(type,val)
#endif

/* Generate code to do memcmp to check if the returned value is in the
   correct location and has the expected value.
   Note that for value that is returned in the caller-allocated memory
   block, we get the address from the saved x8 register.  x8 is saved
   just after the callee is returned; we assume that x8 has not been
   clobbered at then, although there is no requirement for the callee
   preserve the value stored in x8.  Luckily, all test cases here are
   simple enough that x8 doesn't normally get clobbered (although not
   guaranteed).  */
#undef FUNC_VAL_CHECK
#define FUNC_VAL_CHECK(id, type, val, offset, layout)			\
void TEST_FUNC_NAME(id)(char* stack)					\
{									\
  type __x = val;							\
  char* addr;								\
  DUMP_STATUS(type,val)							\
  if (offset != X8)							\
    addr = stack + offset;						\
  else									\
    addr = *(char **)(stack + X8);					\
  if (validate_memory (&__x, addr, sizeof (type), layout) != 0)		\
    abort();								\
}

/* Composite larger than 16 bytes is replaced by a pointer to a copy prepared
   by the caller, so here we extrat the pointer, deref it and compare the
   content with that of the original one.  */
#define PTR(type, val, offset, ...) {					\
  type * ptr;								\
  DUMP_ARG(type,val)							\
  ptr = *(type **)(stack + offset);					\
  if (memcmp (ptr, &val, sizeof (type)) != 0) abort ();			\
}

#include TESTFILE

MYFUNCTYPE myfunc () PCSATTR;

/* Define the function to return VAL of type TYPE.  I and D in the
   parameter list are two dummy parameters to help improve the detection
   of bugs like a short vector being returned in X0 after copied from V0.  */
#undef FUNC_VAL_CHECK
#define FUNC_VAL_CHECK(id, type, var, offset, layout)			  \
__attribute__ ((noipa)) type FUNC_NAME (id) (int i, double d, type t)	  \
  {									  \
    asm (""::"r" (i),"r" (d)); /* asm prevents function from getting      \
				  optimized away.  Using i and d prevents \
				  warnings about unused parameters.	  \
			       */					  \
    /* We save and set up the LR register in a way that essentially	  \
       inserts myfunc () between the return of this function and the	  \
       continuing execution of its caller.  By doing this, myfunc ()	  \
       can save and check the exact content of the registers that are	  \
       used for the function return value.				  \
       The previous approach of sequentially calling myfunc right after	  \
       this function does not guarantee myfunc see the exact register	  \
       content, as compiler may emit code in between the two calls,	  \
       especially during the -O0 codegen.  */				  \
    asm volatile ("mov %0, x30" : "=r" (saved_return_address));		  \
    asm volatile ("mov x30, %0" : : "r" ((unsigned long long) myfunc));   \
    return t;								  \
  }
#include TESTFILE


/* Call the function to return value and call the checking function
   to validate.  See the comment above for the reason of having 0 and 0.0
   in the function argument list.  */
#undef FUNC_VAL_CHECK
#define FUNC_VAL_CHECK(id, type, var, offset, layout)			\
  {									\
    testfunc_ptr = TEST_FUNC_NAME(id);					\
    FUNC_NAME(id) (0, 0.0, var);					\
    /* The above function implicitly calls myfunc () on its return,	\
       and the execution resumes from here after myfunc () finishes.  */\
  }

int main()
{
  which_kind_of_test = TK_RETURN;

#ifdef HAS_DATA_INIT_FUNC
  init_data ();
#endif

#include TESTFILE

  return 0;
}
