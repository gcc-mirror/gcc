/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O3" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

/* Define this after PR89424 is addressed.  */
#define PR89424

/* Define this after PR89626 is addressed.  */
#undef PR89626

#ifdef PR89626
#define SIGNED
#else
#define SIGNED signed
#endif

extern void abort (void);

#define CONST0		(((SIGNED __int128) 31415926539) << 60)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
SIGNED __int128 e0 (vector SIGNED __int128 v)
{
  return __builtin_vec_ext_v1ti (v, 0);
}

SIGNED __int128 e3 (vector SIGNED __int128 v)
{
  return __builtin_vec_ext_v1ti (v, 3);
}

/* Test for vector residing in memory.  */
SIGNED __int128 me0 (vector SIGNED __int128 *vp)
{
  return __builtin_vec_ext_v1ti (*vp, 0);
}

SIGNED __int128 me3 (vector SIGNED __int128 *vp)
{
  return __builtin_vec_ext_v1ti (*vp, 3);
}

/* Test the same with variable indices.  */

#ifdef PR89424
/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
SIGNED __int128 ei (vector SIGNED __int128 v, int i)
{
  return __builtin_vec_ext_v1ti (v, i);
}

/* Test for variable selector and vector residing in memory.  */
SIGNED __int128 mei (vector SIGNED __int128 *vp, int i)
{
  return __builtin_vec_ext_v1ti (*vp, i);
}
#endif

int main (int argc, char *argv[]) {
  vector SIGNED __int128 dv = { CONST0 };
  SIGNED __int128 d;

  d = e0 (dv);
  if (d != CONST0)
    abort ();

  d = e3 (dv);
  if (d != CONST0)
    abort ();

  d = me0 (&dv);
  if (d != CONST0)
    abort ();

  d = me3 (&dv);
  if (d != CONST0)
    abort ();

#ifdef PR89424
  d = ei (dv, 0);
  if (d != CONST0)
    abort ();

  d = ei (dv, 1);
  if (d != CONST0)
    abort ();

  d = ei (dv, 2);
  if (d != CONST0)
    abort ();

  d = ei (dv, 3);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 0);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 1);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 2);
  if (d != CONST0)
    abort ();

  d = mei (&dv, 3);
  if (d != CONST0)
    abort ();
#endif

  return 0;
}
