/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/dfp
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
unsigned int s3 (vector unsigned int v)
{
  return __builtin_vec_extract (v, 3);
}

unsigned int s1 (vector unsigned int v)
{
  return __builtin_vec_extract (v, 1);
}

unsigned int s21 (vector unsigned int v)
{
  return __builtin_vec_extract (v, 21);
}

unsigned int s30 (vector unsigned int v)
{
  return __builtin_vec_extract (v, 30);
}

/* Test for vector residing in memory.  */
unsigned int ms3 (vector unsigned int *vp)
{
  return __builtin_vec_extract (*vp, 3);
}

unsigned int ms1(vector unsigned int *vp)
{
  return __builtin_vec_extract (*vp, 1);
}

unsigned int ms21(vector unsigned int *vp)
{
  return __builtin_vec_extract (*vp, 21);
}

unsigned int ms30(vector unsigned int *vp)
{
  return __builtin_vec_extract (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
unsigned int ci (vector unsigned int v, int i)
{
  return __builtin_vec_extract (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
unsigned int mci(vector unsigned int *vp, int i)
{
  return __builtin_vec_extract (*vp, i);
}


unsigned int main (int argc, unsigned char *argv[]) {
  vector unsigned int sv = { CONST0, CONST1, CONST2, CONST3 };
  unsigned int s;

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
