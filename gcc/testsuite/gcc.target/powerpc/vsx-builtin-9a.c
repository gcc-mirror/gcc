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
signed char c0 (vector signed char v)
{
  return __builtin_vec_ext_v16qi (v, 0);
}

signed char c9 (vector signed char v)
{
  return __builtin_vec_ext_v16qi (v, 9);
}

signed char c21 (vector signed char v)
{
  return __builtin_vec_ext_v16qi (v, 21);
}

signed char c30 (vector signed char v)
{
  return __builtin_vec_ext_v16qi (v, 30);
}

/* Test for vector residing in memory.  */
signed char mc0 (vector signed char *vp)
{
  return __builtin_vec_ext_v16qi (*vp, 0);
}

signed char mc9 (vector signed char *vp)
{
  return __builtin_vec_ext_v16qi (*vp, 9);
}

signed char mc21 (vector signed char *vp)
{
  return __builtin_vec_ext_v16qi (*vp, 21);
}

signed char mc30 (vector signed char *vp)
{
  return __builtin_vec_ext_v16qi (*vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
signed char ci (vector signed char v, int i)
{
  return __builtin_vec_ext_v16qi (v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
signed char mci(vector signed char *vp, int i) {
  return __builtin_vec_ext_v16qi (*vp, i);
}


int main (int argc, char *argv[]) {
  vector signed char cv = { CONST0, CONST1, CONST2, CONST3,
			    CONST4, CONST5, CONST6, CONST7,
			    CONST8, CONST9, CONSTA, CONSTB,
			    CONSTC, CONSTD, CONSTE, CONSTF };
  signed char c;

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
