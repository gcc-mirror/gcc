/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/vmx
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
vector signed char c0 (vector signed char v, signed char x)
{
  return vec_insert (x, v, 0);
}

vector signed char c9 (vector signed char v, signed char x)
{
  return vec_insert (x, v, 9);
}

vector signed char c21 (vector signed char v, signed char x)
{
  return vec_insert (x, v, 21);
}

vector signed char c30 (vector signed char v, signed char x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector signed char mc0 (vector signed char *vp, signed char x)
{
  return vec_insert (x, *vp, 0);
}

vector signed char mc9 (vector signed char *vp, signed char x)
{
  return vec_insert (x, *vp, 9);
}

vector signed char mc21 (vector signed char *vp, signed char x)
{
  return vec_insert (x, *vp, 21);
}

vector signed char mc30 (vector signed char *vp, signed char x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector signed char ci (vector signed char v, int i, signed char x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector signed char mci(vector signed char *vp, int i, signed char x) {
  return vec_insert (x, *vp, i);
}


int main (int argc, char *argv[]) {
  vector signed char cv = { CONST0, CONST1, CONST2, CONST3,
			    CONST4, CONST5, CONST6, CONST7,
			    CONST8, CONST9, CONSTA, CONSTB,
			    CONSTC, CONSTD, CONSTE, CONSTF };
  signed char c;

  cv = c0 (cv, CONSTF);
  if (cv [0] != CONSTF)
    abort ();

  cv = c9 (cv, CONST7);
  if (cv [9] != CONST7)
    abort ();

  cv = c21 (cv, CONSTA);
  if (cv [5] != CONSTA)
    abort ();

  cv = c30 (cv, CONSTC);
  if (cv [14] != CONSTC)
    abort ();

  cv = mc0 (&cv, CONSTB);
  if (cv [0] != CONSTB)
    abort ();

  cv = mc9 (&cv, CONST1);
  if (cv [9] != CONST1)
    abort ();

  cv = mc21 (&cv, CONST7);
  if (cv [5] != CONST7)
    abort ();

  cv = mc30 (&cv, CONST2);
  if (cv [14] != CONST2)
    abort ();

  cv = ci (cv, 8, CONST4);
  if (cv [8] != CONST4)
    abort ();

  cv = ci (cv, 13, CONSTB);
  if (cv [13] != CONSTB)
    abort ();

  cv = ci (cv, 23, CONST3);
  if (cv [7] != CONST3)
    abort ();

  cv = ci (cv, 31, CONST2);
  if (cv [15] != CONST2)
    abort ();

  cv = mci (&cv, 5, CONST1);
  if (cv [5] != CONST1)
    abort ();

  cv = mci (&cv, 12, CONST3);
  if (cv [12] != CONST3)
    abort ();

  cv = mci (&cv, 25, CONST5);
  if (cv [9] != CONST5)
    abort ();

  cv = mci (&cv, 16, CONSTD);
  if (cv [0] != CONSTD)
    abort ();

  return 0;
}
