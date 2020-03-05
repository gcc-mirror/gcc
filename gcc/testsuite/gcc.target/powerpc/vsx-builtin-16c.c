/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec" } */

/* This test should run the same on any target that supports altivec/vmx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <stdio.h>
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
vector unsigned char c0 (vector unsigned char v, unsigned char x)
{
  return vec_insert (x, v, 0);
}

vector unsigned char c9 (vector unsigned char v, unsigned char x)
{
  return vec_insert (x, v, 9);
}

vector unsigned char c21 (vector unsigned char v, unsigned char x)
{
  return vec_insert (x, v, 21);
}

vector unsigned char c30 (vector unsigned char v, unsigned char x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector unsigned char mc0 (vector unsigned char *vp, unsigned char x)
{
  return vec_insert (x, *vp, 0);
}

vector unsigned char mc9 (vector unsigned char *vp, unsigned char x)
{
  return vec_insert (x, *vp, 9);
}

vector unsigned char mc21 (vector unsigned char *vp, unsigned char x)
{
  return vec_insert (x, *vp, 21);
}

vector unsigned char mc30 (vector unsigned char *vp, unsigned char x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector unsigned char ci (vector unsigned char v, int i, unsigned char x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector unsigned char mci (vector unsigned char *vp, int i, unsigned char x)
{
  return vec_insert (x, *vp, i);
}


int main (int argc, char *argv[]) {
  vector unsigned char cv = { CONST0, CONST1, CONST2, CONST3,
			      CONST4, CONST5, CONST6, CONST7,
			      CONST8, CONST9, CONSTA, CONSTB,
			      CONSTC, CONSTD, CONSTE, CONSTF };
  printf ("A\n");
  cv = c0 (cv, CONST3);
  if (cv [0] != CONST3)
    abort ();

  printf ("B\n");
  cv = c9 (cv, CONST2);
  if (cv [9] != CONST2)
    abort ();

  printf ("C\n");
  cv = c21 (cv, CONSTF);
  if (cv [5] != CONSTF)
    abort ();

  printf ("D\n");
  cv = c30 (cv, CONST3);
  if (cv [14] != CONST3)
    abort ();

  printf ("E\n");
  cv = mc0 (&cv, CONST4);
  if (cv [0] != CONST4)
    abort ();

  printf ("F\n");
  cv = mc9 (&cv, CONST3);
  if (cv [9] != CONST3)
    abort ();

  printf ("G\n");
  cv = mc21 (&cv, CONST1);
  if (cv [5] != CONST1)
    abort ();

  printf ("H\n");
  cv = mc30 (&cv, CONSTC);
  if (cv [14] != CONSTC)
    abort ();

  printf ("I\n");
  cv = ci (cv, 8, CONSTD);
  if (cv [8] != CONSTD)
    abort ();

  printf ("J\n");
  cv = ci (cv, 13, CONST5);
  if (cv [13] != CONST5)
    abort ();

  printf ("K\n");
  cv = ci (cv, 23, CONST6);
  if (cv [7] != CONST6)
    abort ();

  printf ("L\n");
  cv = ci (cv, 31, CONST7);
  if (cv [15] != CONST7)
    abort ();

  printf ("M\n");
  cv = mci (&cv, 5, CONST8);
  if (cv [5] != CONST8)
    abort ();

  printf ("N\n");
  cv = mci (&cv, 12, CONST9);
  if (cv [12] != CONST9)
    abort ();

  printf ("O\n");
  cv = mci (&cv, 25, CONSTA);
  if (cv [9] != CONSTA)
    abort ();

  printf ("P\n");
  cv = mci (&cv, 16, CONSTB);
  if (cv [0] != CONSTB)
    abort ();

  return 0;
}
