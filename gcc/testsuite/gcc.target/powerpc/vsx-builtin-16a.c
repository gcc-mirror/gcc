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
#define CONST8		(8)
#define CONST9		(9)
#define CONSTA		(10)
#define CONSTB		(11)
#define CONSTC		(12)
#define CONSTD		(13)
#define CONSTE		(14)
#define CONSTF		(15)


/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
unsigned char c0 (vector unsigned char v)
{
  return __builtin_vec_extract (v, 0);
}

unsigned char c9 (vector unsigned char v)
{
  return __builtin_vec_extract (v, 9);
}

unsigned char c21 (vector unsigned char v)
{
  return __builtin_vec_extract (v, 21);
}

unsigned char c30 (vector unsigned char v)
{
  return __builtin_vec_extract (v, 30);
}

/* Test for vector residing in memory.  */
unsigned char mc0 (vector unsigned char *vp)
{
  return __builtin_vec_extract (*vp, 0);
}

unsigned char mc9 (vector unsigned char *vp)
{
  return __builtin_vec_extract (*vp, 9);
}

unsigned char mc21 (vector unsigned char *vp)
{
  return __builtin_vec_extract (*vp, 21);
}

unsigned char mc30 (vector unsigned char *vp)
{
  return __builtin_vec_extract (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
unsigned char ci (vector unsigned char v, int i)
{
  return __builtin_vec_extract (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
unsigned char mci (vector unsigned char *vp, int i)
{
  return __builtin_vec_extract (*vp, i);
}


int main (int argc, char *argv[]) {
  vector unsigned char cv = { CONST0, CONST1, CONST2, CONST3,
			    CONST4, CONST5, CONST6, CONST7,
			    CONST8, CONST9, CONSTA, CONSTB,
			    CONSTC, CONSTD, CONSTE, CONSTF };
  unsigned char c;

  c = c0 (cv);
  if (c != CONST0)
    abort ();

  c = c9 (cv);
  if (c != CONST9)
    abort ();

  c = c21 (cv);
  if (c != CONST5)
    abort ();

  c = c30 (cv);
  if (c != CONSTE)
    abort ();

  c = mc0 (&cv);
  if (c != CONST0)
    abort ();

  c = mc9 (&cv);
  if (c != CONST9)
    abort ();

  c = mc21 (&cv);
  if (c != CONST5)
    abort ();

  c = mc30 (&cv);
  if (c != CONSTE)
    abort ();

  c = ci (cv, 8);
  if (c != CONST8)
    abort ();

  c = ci (cv, 13);
  if (c != CONSTD)
    abort ();

  c = ci (cv, 23);
  if (c != CONST7)
    abort ();

  c = ci (cv, 31);
  if (c != CONSTF)
    abort ();

  c = mci (&cv, 5);
  if (c != CONST5)
    abort ();

  c = mci (&cv, 12);
  if (c != CONSTC)
    abort ();

  c = mci (&cv, 25);
  if (c != CONST9)
    abort ();

  c = mci (&cv, 16);
  if (c != CONST0)
    abort ();

  return 0;
}
