/* Make sure the doloop optimization is done for this loop. */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O2" } */
/* XFAIL for now, see PR66612.  */
/* { dg-final { scan-assembler "bdn" { xfail *-*-* } } } */
extern int a[];
int foo(int w) {
  int n = w;
  while (n >= 512)
    {
    a[n] = 42;
    n -= 256;
    }
  }
