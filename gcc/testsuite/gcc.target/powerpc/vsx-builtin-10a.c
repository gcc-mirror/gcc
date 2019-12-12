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
#define CONST4		(4)
#define CONST5		(5)
#define CONST6		(6)
#define CONST7		(7)


/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
short s3 (vector short v)
{
  return __builtin_vec_ext_v8hi (v, 3);
}

short s7 (vector short v)
{
  return __builtin_vec_ext_v8hi (v, 7);
}

short s21 (vector short v)
{
  return __builtin_vec_ext_v8hi (v, 21);
}

short s30 (vector short v)
{
  return __builtin_vec_ext_v8hi (v, 30);
}

/* Test for vector residing in memory.  */
short ms3 (vector short *vp)
{
  return __builtin_vec_ext_v8hi (*vp, 3);
}

short ms7 (vector short *vp)
{
  return __builtin_vec_ext_v8hi (*vp, 7);
}

short ms21 (vector short *vp)
{
  return __builtin_vec_ext_v8hi (*vp, 21);
}

short ms30 (vector short *vp)
{
  return __builtin_vec_ext_v8hi (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
short ci (vector short v, int i)
{
  return __builtin_vec_ext_v8hi (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
short mci (vector short *vp, int i)
{
  return __builtin_vec_ext_v8hi (*vp, i);
}


int main (int argc, short *argv[]) {
  vector short sv = {
    CONST0, CONST1, CONST2, CONST3, CONST4, CONST5, CONST6, CONST7 };
  short s;

  s = s3 (sv);
  if (s != CONST3)
    abort ();

  s = s7 (sv);
  if (s != CONST7)
    abort ();

  s = s21 (sv);
  if (s != CONST5)
    abort ();

  s = s30 (sv);
  if (s != CONST6)
    abort ();

  s = ms3 (&sv);
  if (s != CONST3)
    abort ();

  s = ms7 (&sv);
  if (s != CONST7)
    abort ();

  s = ms21 (&sv);
  if (s != CONST5)
    abort ();

  s = ms30 (&sv);
  if (s != CONST6)
    abort ();

  s = ci (sv, 5);
  if (s != CONST5)
    abort ();

  s = ci (sv, 2);
  if (s != CONST2)
    abort ();

  s = ci (sv, 15);
  if (s != CONST7)
    abort ();

  s = ci (sv, 28);
  if (s != CONST4)
    abort ();

  s = mci (&sv, 5);
  if (s != CONST5)
    abort ();

  s = mci (&sv, 12);
  if (s != CONST4)
    abort ();

  s = mci (&sv, 25);
  if (s != CONST1)
    abort ();

  s = mci (&sv, 16);
  if (s != CONST0)
    abort ();

  return 0;
}
