/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O3" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		(3.1415926539)
#define CONST1		(3.1415926539 * 2)


/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
double e0(vector double v){ return __builtin_vec_ext_v2df (v, 0); }
double e1(vector double v){ return __builtin_vec_ext_v2df (v, 1); }
double e2(vector double v){ return __builtin_vec_ext_v2df (v, 2); }
double e3(vector double v){ return __builtin_vec_ext_v2df (v, 3); }

/* Test for vector residing in memory.  */
double me0(vector double *vp){ return __builtin_vec_ext_v2df (*vp, 0); }
double me1(vector double *vp){ return __builtin_vec_ext_v2df (*vp, 1); }
double me2(vector double *vp){ return __builtin_vec_ext_v2df (*vp, 2); }
double me3(vector double *vp){ return __builtin_vec_ext_v2df (*vp, 3); }

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
double ei(vector double v, int i){ return __builtin_vec_ext_v2df (v, i); }

/* Test for variable selector and vector residing in memory.  */
double mei(vector double *vp, int i){ return __builtin_vec_ext_v2df (*vp, i); }


int main (int argc, char *argv[]) {
  vector double dv;
  double d;
  dv[0] = CONST0;
  dv[1] = CONST1;

  d = e0 (dv);
  if (d != CONST0)
    abort ();

  d = e1 (dv);
  if (d != CONST1)
    abort ();

  d = e2 (dv);
  if (d != CONST0)
    abort ();

  d = e3 (dv);
  if (d != CONST1)
    abort ();

  d = me0 (&dv);
  if (d != CONST0)
    abort ();

  d = me1 (&dv);
  if (d != CONST1)
    abort ();

  d = me2 (&dv);
  if (d != CONST0)
    abort ();

  d = me3 (&dv);
  if (d != CONST1)
    abort ();

  d = ei (dv, 0);
  if (d != CONST0)
    abort ();

  d = ei (dv, 1);
  if (d != CONST1)
    abort ();

  d = ei (dv, 2);
  if (d != CONST0)
    abort ();

  d = ei (dv, 3);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 0);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 1);
  if (d != CONST1)
    abort ();

  d = mei (&dv, 2);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 3);
  if (d != CONST1)
    abort ();

  return 0;
}
