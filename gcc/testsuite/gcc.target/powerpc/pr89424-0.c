/* { dg-do run { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

/* Define PR89626 after that pr is addressed.  */
#ifdef PR89626
#define SIGNED
#else
#define SIGNED signed
#endif

#define CONST0		(((__int128) 31415926539) << 60)

/* Test that indices > length of vector are applied modulo the vector
   length.  */


/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
__int128 ei (vector SIGNED __int128 v, int i)
{
  return __builtin_vec_ext_v1ti (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__int128 mei (vector SIGNED __int128 *vp, int i)
{
  return __builtin_vec_ext_v1ti (*vp, i);
}

int main (int argc, char *argv[]) {
  vector SIGNED __int128 dv = { CONST0 };
  __int128 d;

  d = ei (dv, 0);
  if (d != CONST0)
    abort ();

  d = ei (dv, 1);
  if (d != CONST0)
    abort ();

  d = ei (dv, 2);
  if (d != CONST0)
    abort ();

  d = ei (dv, 3);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 0);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 1);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 2);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 3);
  if (d != CONST0)
    abort ();

  return 0;
}
