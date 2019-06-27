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


/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector unsigned short s3 (vector unsigned short v, unsigned short x)
{
  return vec_insert (x, v, 3);
}

vector unsigned short s7 (vector unsigned short v, unsigned short x)
{
  return vec_insert (x, v, 7);
}

vector unsigned short s21 (vector unsigned short v, unsigned short x)
{
  return vec_insert (x, v, 21);
}

vector unsigned short s30 (vector unsigned short v, unsigned short x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector unsigned short ms3 (vector unsigned short *vp, unsigned short x)
{
  return vec_insert (x, *vp, 3);
}

vector unsigned short ms7 (vector unsigned short *vp, unsigned short x)
{
  return vec_insert (x, *vp, 7);
}

vector unsigned short ms21 (vector unsigned short *vp, unsigned short x)
{
  return vec_insert (x, *vp, 21);
}

vector unsigned short ms30 (vector unsigned short *vp, unsigned short x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector unsigned short ci (vector unsigned short v, int i, unsigned short x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector unsigned short mci (vector unsigned short *vp, int i, unsigned short x)
{
  return vec_insert (x, *vp, i);
}


int main (int argc, unsigned short *argv[]) {
  vector unsigned short sv = {
    CONST0, CONST1, CONST2, CONST3, CONST4, CONST5, CONST6, CONST7 };

  sv = s3 (sv, CONST1);
  if (sv [3] != CONST1)
    abort ();

  sv = s7 (sv, CONST2);
  if (sv [7] != CONST2)
    abort ();

  sv = s21 (sv, CONST3);
  if (sv [5] != CONST3)
    abort ();

  sv = s30 (sv, CONST4);
  if (sv [6] != CONST4)
    abort ();

  sv = ms3 (&sv, CONST5);
  if (sv [3] != CONST5)
    abort ();

  sv = ms7 (&sv, CONST6);
  if (sv [7] != CONST6)
    abort ();

  sv = ms21 (&sv, CONST7);
  if (sv [5] != CONST7)
    abort ();

  sv = ms30 (&sv, CONST0);
  if (sv [6] != CONST0)
    abort ();

  sv = ci (sv, 5, CONST1);
  if (sv [5] != CONST1)
    abort ();

  sv = ci (sv, 2, CONST3);
  if (sv [2] != CONST3)
    abort ();

  sv = ci (sv, 15, CONST2);
  if (sv [7] != CONST2)
    abort ();

  sv = ci (sv, 28, CONST5);
  if (sv [4] != CONST5)
    abort ();

  sv = mci (&sv, 5, CONST4);
  if (sv [5] != CONST4)
    abort ();

  sv = mci (&sv, 12, CONST6);
  if (sv [4] != CONST6)
    abort ();

  sv = mci (&sv, 25, CONST7);
  if (sv [1] != CONST7)
    abort ();

  sv = mci (&sv, 16, CONST4);
  if (sv [0] != CONST4)
    abort ();

  return 0;
}
