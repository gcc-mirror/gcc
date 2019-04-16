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
unsigned short s3 (vector unsigned short v)
{
  return __builtin_vec_extract (v, 3);
}

unsigned short s7 (vector unsigned short v)
{
  return __builtin_vec_extract (v, 7);
}

unsigned short s21 (vector unsigned short v)
{
  return __builtin_vec_extract (v, 21);
}

unsigned short s30 (vector unsigned short v)
{
  return __builtin_vec_extract (v, 30);
}

/* Test for vector residing in memory.  */
unsigned short ms3 (vector unsigned short *vp)
{
  return __builtin_vec_extract (*vp, 3);
}

unsigned short ms7 (vector unsigned short *vp)
{
  return __builtin_vec_extract (*vp, 7);
}

unsigned short ms21 (vector unsigned short *vp)
{
  return __builtin_vec_extract (*vp, 21);
}

unsigned short ms30 (vector unsigned short *vp)
{
  return __builtin_vec_extract (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
unsigned short ci (vector unsigned short v, int i)
{
  return __builtin_vec_extract (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
unsigned short mci (vector unsigned short *vp, int i)
{
  return __builtin_vec_extract (*vp, i);
}


int main (int argc, unsigned short *argv[]) {
  vector unsigned short sv = {
    CONST0, CONST1, CONST2, CONST3, CONST4, CONST5, CONST6, CONST7 };
  unsigned short s;

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
