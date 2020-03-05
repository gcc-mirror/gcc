/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O3" } */

/* This test should run the same on any target that supports altivec/vmx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		(0)
#define CONST1		(1)
#define CONST2		(2)
#define CONST3		(3)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector int s3 (vector int v, int x)
{
  return vec_insert (x, v, 3);
}

vector int s1 (vector int v, int x)
{
  return vec_insert (x, v, 1);
}

vector int s21 (vector int v, int x)
{
  return vec_insert (x, v, 21);
}

vector int s30 (vector int v, int x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector int ms3  (vector int *vp, int x)
{
  return vec_insert (x, *vp, 3);
}

vector int ms1 (vector int *vp, int x)
{
  return vec_insert (x, *vp, 1);
}

vector int ms21 (vector int *vp, int x)
{
  return vec_insert (x, *vp, 21);
}

vector int ms30 (vector int *vp, int x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector int ci (vector int v, int i, int x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector int mci(vector int *vp, int i, int x)
{
  return vec_insert (x, *vp, i);
}


int main (int argc, int *argv[]) {
  vector int sv = { CONST0, CONST1, CONST2, CONST3 };
  int s;

  sv = s3 (sv, CONST1);
  if (sv [3] != CONST1)
    abort ();

  sv = s1 (sv, CONST3);
  if (sv [1] != CONST3)
    abort ();

  sv = s21 (sv, CONST0);
  if (sv [1] != CONST0)
    abort ();

  sv = s30 (sv, CONST1);
  if (sv [2] != CONST1)
    abort ();

  sv = ms3 (&sv, CONST2);
  if (sv [3] != CONST2)
    abort ();

  sv = ms1 (&sv, CONST0);
  if (sv [1] != CONST0)
    abort ();

  sv = ms21 (&sv, CONST3);
  if (sv [1] != CONST3)
    abort ();

  sv = ms30 (&sv, CONST0);
  if (sv [2] != CONST0)
    abort ();

  sv = ci (sv, 5, CONST0);
  if (sv [1] != CONST0)
    abort ();

  sv = ci (sv, 2, CONST3);
  if (sv [2] != CONST3)
    abort ();

  sv = ci (sv, 15, CONST1);
  if (sv [3] != CONST1)
    abort ();

  sv = ci (sv, 28, CONST3);
  if (sv [0] != CONST3)
    abort ();

  sv = mci (&sv, 5, CONST2);
  if (sv [1] != CONST2)
    abort ();

  sv = mci (&sv, 12, CONST1);
  if (sv [0] != CONST1)
    abort ();

  sv = mci (&sv, 25, CONST2);
  if (sv [1] != CONST2)
    abort ();

  sv = mci (&sv, 16, CONST3);
  if (sv [0] != CONST3)
    abort ();

  return 0;
}
