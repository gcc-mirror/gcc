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
#define CONST4		(5 * 31415926539LL)
#define CONST5		(6 * 31415926539LL)
#define CONST6		(7 * 31415926539LL)
#define CONST7		(8 * 31415926539LL)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector unsigned long long int
e0 (vector unsigned long long int v, unsigned long long int x)
{
  return vec_insert (x, v, 0);
}

vector unsigned long long int
e3 (vector unsigned long long int v, unsigned long long int x)
{
  return vec_insert (x, v, 3);
}

/* Test for vector residing in memory.  */
vector unsigned long long int
me0 (vector unsigned long long int *vp, unsigned long long int x)
{
  return vec_insert (x, *vp, 0);
}

vector unsigned long long int
me3 (vector unsigned long long int *vp, unsigned long long int x)
{
  return vec_insert (x, *vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector unsigned long long int
ei (vector unsigned long long int v, int i, unsigned long long int x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector unsigned long long int
mei (vector unsigned long long int *vp, int i, unsigned long long int x)
{
  return vec_insert (x, *vp, i);
}

int main (int argc, char *argv[]) {
  vector unsigned long long int dv = { CONST0, CONST1 };
  unsigned long long int d;

  dv = e0 (dv, CONST7);
  if (dv [0] != CONST7)
    abort ();

  dv = e3 (dv, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = me0 (&dv, CONST4);
  if (dv [0] != CONST4)
    abort ();

  dv = me3 (&dv, CONST3);
  if (dv [1] != CONST3)
    abort ();

  dv = ei (dv, 0, CONST5);
  if (dv [0] != CONST5)
    abort ();

  dv = ei (dv, 1, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = ei (dv, 2, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = ei (dv, 3, CONST6);
  if (dv [1] != CONST6)
    abort ();

  dv = mei (&dv, 0, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 1, CONST3);
  if (dv [1] != CONST3)
    abort ();

  dv = mei (&dv, 2, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 3, CONST2);
  if (dv [1] != CONST2)
    abort ();

  return 0;
}
