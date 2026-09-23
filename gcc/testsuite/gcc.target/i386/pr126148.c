/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -ffast-math -mapxf -mno-80387 -mfpmath=387" } */

int foo (int a, double b) {
  if (a || b)
    return 1;
  return 0;
}
