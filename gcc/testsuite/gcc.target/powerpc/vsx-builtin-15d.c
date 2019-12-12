/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O3" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		(3.1415926539)
#define CONST1		(3.1415926539 * 2)
#define CONST2		(3.1415926539 * 3)
#define CONST3		(3.1415926539 * 4)


/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector double e0(vector double v, double x)
{
  return vec_insert (x, v, 0);
}

vector double e1(vector double v, double x)
{
  return vec_insert (x, v, 1);
}

vector double e2(vector double v, double x)
{
  return vec_insert (x, v, 2);
}

vector double e3(vector double v, double x)
{
  return vec_insert (x, v, 3);
}

/* Test for vector residing in memory.  */
vector double me0(vector double *vp, double x)
{
  return vec_insert (x, *vp, 0);
}

vector double me1(vector double *vp, double x)
{
  return vec_insert (x, *vp, 1);
}

vector double me2(vector double *vp, double x)
{
  return vec_insert (x, *vp, 2);
}

vector double me3(vector double *vp, double x)
{
  return vec_insert (x, *vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector double ei(vector double v, int i, double x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector double mei(vector double *vp, int i, double x)
{
  return vec_insert (x, *vp, i);
}

int main (int argc, char *argv[]) {
  vector double dv;
  double d;
  dv[0] = CONST0;
  dv[1] = CONST1;

  dv = e0 (dv, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = e1 (dv, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = e2 (dv, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = e3 (dv, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = me0 (&dv, CONST2);
  if (dv [0] != CONST2)
    abort ();

  dv = me1 (&dv, CONST3);
  if (dv [1] != CONST3)
    abort ();

  dv = me2 (&dv, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = me3 (&dv, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = ei (dv, 0, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = ei (dv, 1, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = ei (dv, 2, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = ei (dv, 3, CONST2);
  if (dv [1] != CONST2)
    abort ();

  dv = mei (&dv, 0, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 1, CONST0);
  if (dv [1] != CONST0)
    abort ();

  dv = mei (&dv, 2, CONST2);
  if (dv [0] != CONST2)
    abort ();

  dv = mei (&dv, 3, CONST3);
  if (dv [1] != CONST3)
    abort ();

  return 0;
}
