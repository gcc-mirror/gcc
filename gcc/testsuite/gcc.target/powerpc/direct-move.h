/* Test functions for direct move support.  */

#include <math.h>
#include <string.h>
#include <stdlib.h>
extern void abort (void);

#ifndef VSX_REG_ATTR
#define VSX_REG_ATTR "wa"
#endif

void __attribute__((__noinline__))
copy (TYPE *a, TYPE *b)
{
  *b = *a;
}

#ifndef NO_GPR
void __attribute__((__noinline__))
load_gpr (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  __asm__ ("# gpr, reg = %0" : "+b" (c));
  *b = c;
}
#endif

#ifndef NO_FPR
void __attribute__((__noinline__))
load_fpr (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  __asm__ ("# fpr, reg = %0" : "+d" (c));
  *b = c;
}
#endif

#ifndef NO_ALTIVEC
void __attribute__((__noinline__))
load_altivec (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  __asm__ ("# altivec, reg = %0" : "+v" (c));
  *b = c;
}
#endif

#ifndef NO_VSX
void __attribute__((__noinline__))
load_vsx (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  __asm__ ("# vsx, reg = %x0" : "+" VSX_REG_ATTR (c));
  *b = c;
}
#endif

#ifndef NO_GPR_TO_VSX
void __attribute__((__noinline__))
load_gpr_to_vsx (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  TYPE d;
  __asm__ ("# gpr, reg = %0" : "+b" (c));
  d = c;
  __asm__ ("# vsx, reg = %x0" : "+" VSX_REG_ATTR (d));
  *b = d;
}
#endif

#ifndef NO_VSX_TO_GPR
void __attribute__((__noinline__))
load_vsx_to_gpr (TYPE *a, TYPE *b)
{
  TYPE c = *a;
  TYPE d;
  __asm__ ("# vsx, reg = %x0" : "+" VSX_REG_ATTR (c));
  d = c;
  __asm__ ("# gpr, reg = %0" : "+b" (d));
  *b = d;
}
#endif

#ifdef DO_MAIN
typedef void (fn_type (TYPE *, TYPE *));

struct test_struct {
  fn_type *func;
  const char *name;
};

const struct test_struct test_functions[] = {
  { copy,		"copy"		  },
#ifndef NO_GPR
  { load_gpr,		"load_gpr"	  },
#endif
#ifndef NO_FPR
  { load_fpr,		"load_fpr"	  },
#endif
#ifndef NO_ALTIVEC
  { load_altivec,	"load_altivec"	  },
#endif
#ifndef NO_VSX
  { load_vsx,		"load_vsx"	  },
#endif
#ifndef NO_GPR_TO_VSX
  { load_gpr_to_vsx,	"load_gpr_to_vsx" },
#endif
#ifndef NO_VSX_TO_GPR
  { load_vsx_to_gpr,	"load_vsx_to_gpr" },
#endif
};

/* Test a given value for each of the functions.  */
void __attribute__((__noinline__))
test_value (TYPE a)
{
  long i;

  for (i = 0; i < sizeof (test_functions) / sizeof (test_functions[0]); i++)
    {
      TYPE b;

      test_functions[i].func (&a, &b);
      if (memcmp ((void *)&a, (void *)&b, sizeof (TYPE)) != 0)
	abort ();
    }
}

/* Main program.  */
int
main (void)
{
  long i,j;
  union {
    TYPE value;
    unsigned char bytes[sizeof (TYPE)];
  } u;

#if IS_INT
  TYPE value = (TYPE)-5;
  for (i = 0; i < 12; i++)
    {
      test_value (value);
      value++;
    }

  for (i = 0; i < 8*sizeof (TYPE); i++)
    test_value (((TYPE)1) << i);

#elif IS_UNS
  TYPE value = (TYPE)0;
  for (i = 0; i < 10; i++)
    {
      test_value (value);
      test_value (~ value);
      value++;
    }

  for (i = 0; i < 8*sizeof (TYPE); i++)
    test_value (((TYPE)1) << i);

#elif IS_FLOAT
  TYPE value = (TYPE)-5;
  for (i = 0; i < 12; i++)
    {
      test_value (value);
      value++;
    }

  test_value ((TYPE)3.1415926535);
  test_value ((TYPE)1.23456);
  test_value ((TYPE)(-0.0));
  test_value ((TYPE)NAN);
  test_value ((TYPE)+INFINITY);
  test_value ((TYPE)-INFINITY);
#else

  for (j = 0; j < 10; j++)
    {
      for (i = 0; i < sizeof (TYPE); i++)
	u.bytes[i] = (unsigned char) (rand () >> 4);

      test_value (u.value);
    }
#endif

  return 0;
}
#endif
