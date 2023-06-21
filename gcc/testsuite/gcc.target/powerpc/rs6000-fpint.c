/* { dg-options "-mno-powerpc-gfxopt -mdejagnu-cpu=power6" } */
/* { dg-final { scan-assembler-not "stfiwx" } } */

/* A basic test of the old-style (not stfiwx) fp -> int conversion.  */
int f(double a, double b)
{
  int a1 = a;
  int b1 = b;
  return a1+b1;
}
