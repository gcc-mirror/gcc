/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/dfp
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
int s3 (vector int v)
{
  return __builtin_vec_ext_v4si (v, 3);
}

int s1 (vector int v)
{
  return __builtin_vec_ext_v4si (v, 1);
}

int s21 (vector int v)
{
  return __builtin_vec_ext_v4si (v, 21);
}

int s30 (vector int v)
{
  return __builtin_vec_ext_v4si (v, 30);
}

/* Test for vector residing in memory.  */
int ms3 (vector int *vp)
{
  return __builtin_vec_ext_v4si (*vp, 3);
}

int ms1(vector int *vp)
{
  return __builtin_vec_ext_v4si (*vp, 1);
}

int ms21(vector int *vp)
{
  return __builtin_vec_ext_v4si (*vp, 21);
}

int ms30(vector int *vp)
{
  return __builtin_vec_ext_v4si (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
int ci (vector int v, int i)
{
  return __builtin_vec_ext_v4si (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
int mci(vector int *vp, int i)
{
  return __builtin_vec_ext_v4si (*vp, i);
}


int main (int argc, int *argv[]) {
  vector int sv = { CONST0, CONST1, CONST2, CONST3 };
  int s;

  s = s3 (sv);
  if (s != CONST3)
    abort ();

  s = s1 (sv);
  if (s != CONST1)
    abort ();

  s = s21 (sv);
  if (s != CONST1)
    abort ();

  s = s30 (sv);
  if (s != CONST2)
    abort ();

  s = ms3 (&sv);
  if (s != CONST3)
    abort ();

  s = ms1 (&sv);
  if (s != CONST1)
    abort ();

  s = ms21 (&sv);
  if (s != CONST1)
    abort ();

  s = ms30 (&sv);
  if (s != CONST2)
    abort ();

  s = ci (sv, 5);
  if (s != CONST1)
    abort ();

  s = ci (sv, 2);
  if (s != CONST2)
    abort ();

  s = ci (sv, 15);
  if (s != CONST3)
    abort ();

  s = ci (sv, 28);
  if (s != CONST0)
    abort ();

  s = mci (&sv, 5);
  if (s != CONST1)
    abort ();

  s = mci (&sv, 12);
  if (s != CONST0)
    abort ();

  s = mci (&sv, 25);
  if (s != CONST1)
    abort ();

  s = mci (&sv, 16);
  if (s != CONST0)
    abort ();

  return 0;
}
