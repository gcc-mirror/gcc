/* { dg-do run } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O3" } */

/* This test should run the same on any target that supports altivec/dfp
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		((float) (3.1415926539))
#define CONST1		((float) (3.1415926539 * 2))
#define CONST2		((float) (3.1415926539 * 3))
#define CONST3		((float) (3.1415926539 * 4))

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
float e0(vector float v){ return __builtin_vec_ext_v4sf (v, 0); }
float e1(vector float v){ return __builtin_vec_ext_v4sf (v, 1); }
float e7(vector float v){ return __builtin_vec_ext_v4sf (v, 7); }
float e8(vector float v){ return __builtin_vec_ext_v4sf (v, 8); }

/* Test for vector residing in memory.  */
float me0(vector float *vp){ return __builtin_vec_ext_v4sf (*vp, 0); }
float me1(vector float *vp){ return __builtin_vec_ext_v4sf (*vp, 1); }

float me13(vector float *vp)
{
  return __builtin_vec_ext_v4sf (*vp, 13);
}

float me15(vector float *vp)
{
  return __builtin_vec_ext_v4sf (*vp, 15);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
float ei(vector float v, int i)
{
  return __builtin_vec_ext_v4sf (v, i);
}

/* Test for variable selector and vector residing in memory.  */
float mei(vector float *vp, int i)
{
  return __builtin_vec_ext_v4sf (*vp, i);
}


int main (int argc, char *argv[]) {
  vector float dv = { CONST0, CONST1, CONST2, CONST3 };
  float d;

  d = e0 (dv);
  if (d != CONST0)
    abort ();

  d = e1 (dv);
  if (d != CONST1)
    abort ();

  d = e7 (dv);
  if (d != CONST3)
    abort ();

  d = e8 (dv);
  if (d != CONST0)
    abort ();

  d = me0 (&dv);
  if (d != CONST0)
    abort ();

  d = me1 (&dv);
  if (d != CONST1)
    abort ();

  d = me13 (&dv);
  if (d != CONST1)
    abort ();

  d = me15 (&dv);
  if (d != CONST3)
    abort ();

  d = ei (dv, 0);
  if (d != CONST0)
    abort ();

  d = ei (dv, 2);
  if (d != CONST2)
    abort ();

  d = ei (dv, 11);
  if (d != CONST3)
    abort ();

  d = ei (dv, 17);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 0);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 1);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 15);
  if (d != CONST3)
    abort ();

  d = mei (&dv, 6);
  if (d != CONST2)
    abort ();

  return 0;
}
