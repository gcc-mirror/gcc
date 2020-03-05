/* { dg-do run { target int128 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx" } */

/* This test should run the same on any target that supports vsx
   instructions.  Intentionally not specifying cpu in order to test
   all code generation paths.  */

#include <altivec.h>

extern void abort (void);

#define CONST0		(((unsigned __int128) 31415926539) << 60)
#define CONST1		(((unsigned __int128) 31415926539) << 54)
#define CONST2		(((unsigned __int128) 31415926539) << 48)
#define CONST3		(((unsigned __int128) 31415926539) << 32)

/* Test that indices > length of vector are applied modulo the vector
   length.  */

/* Test for vector residing in register.  */
vector unsigned __int128 e0 (vector unsigned __int128 v, unsigned __int128 x)
{
  return vec_insert (x, v, 0);
}

vector unsigned __int128 e3 (vector unsigned __int128 v, unsigned __int128 x)
{
  return vec_insert (x, v, 3);
}

/* Test for vector residing in memory.  */
vector unsigned __int128
me0 (vector unsigned __int128 *vp, unsigned __int128 x)
{
  return vec_insert (x, *vp, 0);
}

vector unsigned __int128
me3 (vector unsigned __int128 *vp, unsigned __int128 x)
{
  return vec_insert (x, *vp, 3);
}

/* Test the same with variable indices.  */

/* Test for variable selector and vector residing in register.  */
__attribute__((noinline))
vector unsigned __int128
ei (vector unsigned __int128 v, int i, unsigned __int128 x)
{
  return vec_insert (x, v, i);
}

/* Test for variable selector and vector residing in memory.  */
__attribute__((noinline))
vector unsigned __int128
mei (vector unsigned __int128 *vp, int i, unsigned __int128 x)
{
  return vec_insert (x, *vp, i);
}

int main (int argc, char *argv[]) {
  vector unsigned __int128 dv = { CONST0 };

  dv = e0 (dv, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = e3 (dv, CONST2);
  if (dv [0] != CONST2)
    abort ();

  dv = me0 (&dv, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = me3 (&dv, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = ei (dv, 0, CONST0);
  if (dv [0] != CONST0)
    abort ();

  dv = ei (dv, 1, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = ei (dv, 2, CONST2);
  if (dv [0] != CONST2)
    abort ();

  dv = ei (dv, 3, CONST3);
  if (dv [0] != CONST3)
    abort ();

  dv = mei (&dv, 0, CONST0);
  if (dv [0] != CONST0)
    abort ();

  dv = mei (&dv, 1, CONST1);
  if (dv [0] != CONST1)
    abort ();

  dv = mei (&dv, 2, CONST2);
  if (dv [0] != CONST2)
    abort ();

  dv = mei (&dv, 3, CONST3);
  if (dv [0] != CONST3)
    abort ();

  return 0;
}
