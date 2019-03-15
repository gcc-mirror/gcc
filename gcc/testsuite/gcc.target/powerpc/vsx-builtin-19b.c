/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O3" } */

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
unsigned long long int e0 (vector unsigned long long int v)
{
  return __builtin_vec_extract (v, 0);
}

unsigned long long int e3 (vector unsigned long long int v)
{
  return __builtin_vec_extract (v, 3);
}

/* Test for vector residing in memory.  */
unsigned long long int me0 (vector unsigned long long int *vp)
{
  return __builtin_vec_extract (*vp, 0);
}

unsigned long long int me3 (vector unsigned long long int *vp)
{
  return __builtin_vec_extract (*vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
unsigned long long int ei (vector unsigned long long int v, int i)
{
  return __builtin_vec_extract (v, i);
}

/* Test for variable selector and vector residing in memory.  */
unsigned long long int mei (vector unsigned long long int *vp, int i)
{
  return __builtin_vec_extract (*vp, i);
}

int main (int argc, char *argv[]) {
  vector unsigned long long int dv = { CONST0, CONST1 };
  unsigned long long int d;

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
