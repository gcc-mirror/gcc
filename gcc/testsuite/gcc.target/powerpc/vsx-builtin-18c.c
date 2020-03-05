/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/vmx
   instructions.  Unsigned Intentionally not specifying cpu in order to test
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
vector unsigned int s3 (vector unsigned int v, unsigned int x)
{
  return vec_insert (x, v, 3);
}

vector unsigned int s1 (vector unsigned int v, unsigned int x)
{
  return vec_insert (x, v, 1);
}

vector unsigned int s21 (vector unsigned int v, unsigned int x)
{
  return vec_insert (x, v, 21);
}

vector unsigned int s30 (vector unsigned int v, unsigned int x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector unsigned int ms3 (vector unsigned int *vp, unsigned int x)
{
  return vec_insert (x, *vp, 3);
}

vector unsigned int ms1(vector unsigned int *vp, unsigned int x)
{
  return vec_insert (x, *vp, 1);
}

vector unsigned int ms21(vector unsigned int *vp, unsigned int x)
{
  return vec_insert (x, *vp, 21);
}

vector unsigned int ms30(vector unsigned int *vp, unsigned int x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector unsigned int ci (vector unsigned int v, int i, unsigned int x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector unsigned int mci(vector unsigned int *vp, int i, unsigned int x)
{
  return vec_insert (x, *vp, i);
}


int main (int argc, unsigned char *argv[]) {
  vector unsigned int sv = { CONST0, CONST1, CONST2, CONST3 };

  sv = s3 (sv, CONST2);
  if (sv [3] != CONST2)
    abort ();

  sv = s1 (sv, CONST2);
  if (sv [1] != CONST2)
    abort ();

  sv = s21 (sv, CONST3);
  if (sv [1] != CONST3)
    abort ();

  sv = s30 (sv, CONST1);
  if (sv [2] != CONST1)
    abort ();

  sv = ms3 (&sv, CONST0);
  if (sv [3] != CONST0)
    abort ();

  sv = ms1 (&sv, CONST0);
  if (sv [1] != CONST0)
    abort ();

  sv = ms21 (&sv, CONST1);
  if (sv [1] != CONST1)
    abort ();

  sv = ms30 (&sv, CONST0);
  if (sv [2] != CONST0)
    abort ();

  sv = ci (sv, 5, CONST3);
  if (sv [1] != CONST3)
    abort ();

  sv = ci (sv, 2, CONST0);
  if (sv [2] != CONST0)
    abort ();

  sv = ci (sv, 15, CONST1);
  if (sv [3] != CONST1)
    abort ();

  sv = ci (sv, 28, CONST3);
  if (sv [0] != CONST3)
    abort ();

  sv = mci (&sv, 5, CONST0);
  if (sv [1] != CONST0)
    abort ();

  sv = mci (&sv, 12, CONST2);
  if (sv [0] != CONST2)
    abort ();

  sv = mci (&sv, 25, CONST3);
  if (sv [1] != CONST3)
    abort ();

  sv = mci (&sv, 16, CONST1);
  if (sv [0] != CONST1)
    abort ();

  return 0;
}
