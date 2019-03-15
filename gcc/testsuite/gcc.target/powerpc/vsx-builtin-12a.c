/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		(31415926539LL)
#define CONST1		(2 * 31415926539LL)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
long long int e0 (vector long long int v)
{
  return __builtin_vec_ext_v2di (v, 0);
}

long long int e3 (vector long long int v)
{
  return __builtin_vec_ext_v2di (v, 3);
}

/* Test for vector residing in memory.  */
long long int me0 (vector long long int *vp)
{
  return __builtin_vec_ext_v2di (*vp, 0);
}

long long int me3 (vector long long int *vp)
{
  return __builtin_vec_ext_v2di (*vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
long long int ei (vector long long int v, int i)
{
  return __builtin_vec_ext_v2di (v, i);
}

/* Test for variable selector and vector residing in memory.  */
long long int mei (vector long long int *vp, int i)
{
  return __builtin_vec_ext_v2di (*vp, i);
}

int main (int argc, char *argv[]) {
  vector long long int dv = { CONST0, CONST1 };
  long long int d;

  d = e0 (dv);
  if (d != CONST0)
    abort ();

  d = e3 (dv);
  if (d != CONST1)
    abort ();

  d = me0 (&dv);
  if (d != CONST0)
    abort ();

  d = me3 (&dv);
  if (d != CONST1)
    abort ();

  d = ei (dv, 0);
  if (d != CONST0)
    abort ();

  d = ei (dv, 1);
  if (d != CONST1)
    abort ();

  d = ei (dv, 2);
  if (d != CONST0)
    abort ();

  d = ei (dv, 3);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 0);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 1);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 2);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 3);
  if (d != CONST1)
    abort ();

  return 0;
}
