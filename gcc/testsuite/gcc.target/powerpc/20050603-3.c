/* { dg-do compile } */
/* { dg-options "-O2" } */
struct Q
{
  long x:20;
  long y:4;
  long z:8;
}b;
/* This should generate a single rl[wd]imi. */
void rotins (unsigned int x)
{
  b.y = (x<<12) | (x>>20);
}

/* The XFAILs are PR68803.  */
/* { dg-final { scan-assembler-not {\mrlwinm} { xfail powerpc64le-*-* } } } */
/* { dg-final { scan-assembler-not {\mrldic} } } */
/* { dg-final { scan-assembler-not {\mrot[lr]} } } */
/* { dg-final { scan-assembler-not {\ms[lr][wd]} } } */
/* { dg-final { scan-assembler-times {\mrl[wd]imi} 1 { xfail powerpc64le-*-* } } } */
