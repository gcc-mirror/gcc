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
#define CONST2		(3 * 31415926539LL)
#define CONST3		(4 * 31415926539LL)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector long long int e0 (vector long long int v, long long int x)
{
  return vec_insert (x, v, 0);
}

vector long long int e3 (vector long long int v, long long int x)
{
  return vec_insert (x, v, 3);
}

/* Test for vector residing in memory.  */
vector long long int me0 (vector long long int *vp, long long int x)
{
  return vec_insert (x, *vp, 0);
}

vector long long int me3 (vector long long int *vp, long long int x)
{
  return vec_insert (x, *vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector long long int ei (vector long long int v, int i, long long int x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector long long int mei (vector long long int *vp, int i, long long int x)
{
  return vec_insert (x, *vp, i);
}

int main (int argc, char *argv[]) {
  vector long long int dv = { CONST0, CONST1 };
  long long int d;

  dv = e0 (dv, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = e3 (dv, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = me0 (&dv, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = me3 (&dv, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = ei (dv, 0, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = ei (dv, 1, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = ei (dv, 2, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = ei (dv, 3, CONST3);
  if (dv [1] != CONST3)
    abort ();

  dv = mei (&dv, 0, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 1, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = mei (&dv, 2, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 3, CONST2);
  if (dv [1] != CONST2)
    abort ();

  return 0;
}
