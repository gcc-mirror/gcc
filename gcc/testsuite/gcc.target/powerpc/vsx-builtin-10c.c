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
vector short s3 (vector short v, short x)
{
  return vec_insert (x, v, 3);
}

vector short s7 (vector short v, short x)
{
  return vec_insert (x, v, 7);
}

vector short s21 (vector short v, short x)
{
  return vec_insert (x, v, 21);
}

vector short s30 (vector short v, short x)
{
  return vec_insert (x, v, 30);
}

/* Test for vector residing in memory.  */
vector short ms3 (vector short *vp, short x)
{
  return vec_insert (x, *vp, 3);
}

vector short ms7 (vector short *vp, short x)
{
  return vec_insert (x, *vp, 7);
}

vector short ms21 (vector short *vp, short x)
{
  return vec_insert (x, *vp, 21);
}

vector short ms30 (vector short *vp, short x)
{
  return vec_insert (x, *vp, 30);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector short ci (vector short v, int i, short x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector short mci (vector short *vp, int i, short x)
{
  return vec_insert (x, *vp, i);
}


int main (int argc, short *argv[]) {
  vector short sv = {
    CONST0, CONST1, CONST2, CONST3, CONST4, CONST5, CONST6, CONST7 };
  short s;

  sv = s3 (sv, CONST6);
  if (sv [3] != CONST6)
    abort ();

  sv = s7 (sv, CONST4);
  if (sv [7] != CONST4)
    abort ();

  sv = s21 (sv, CONST3);
  if (sv [5] != CONST3)
    abort ();

  sv = s30 (sv, CONST2);
  if (sv [6] != CONST2)
    abort ();

  sv = ms3 (&sv, CONST5);
  if (sv [3] != CONST5)
    abort ();

  sv = ms7 (&sv, CONST1);
  if (sv [7] != CONST1)
    abort ();

  sv = ms21 (&sv, CONST2);
  if (sv [5] != CONST2)
    abort ();

  sv = ms30 (&sv, CONST0);
  if (sv [6] != CONST0)
    abort ();

  sv = ci (sv, 5, CONST6);
  if (sv [5] != CONST6)
    abort ();

  sv = ci (sv, 2, CONST4);
  if (sv [2] != CONST4)
    abort ();

  sv = ci (sv, 15, CONST3);
  if (sv [7] != CONST3)
    abort ();

  sv = ci (sv, 28, CONST3);
  if (sv [4] != CONST3)
    abort ();

  sv = mci (&sv, 5, CONST3);
  if (sv [5] != CONST3)
    abort ();

  sv = mci (&sv, 12, CONST7);
  if (sv [4] != CONST7)
    abort ();

  sv = mci (&sv, 25, CONST6);
  if (sv [1] != CONST6)
    abort ();

  sv = mci (&sv, 16, CONST5);
  if (sv [0] != CONST5)
    abort ();

  return 0;
}
