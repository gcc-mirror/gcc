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

/* { dg-final { scan-assembler-not {\mrlwinm} } } */
/* { dg-final { scan-assembler-not {\mrldic} } } */
/* { dg-final { scan-assembler-not {\mrot[lr]} } } */
/* { dg-final { scan-assembler-not {\ms[lr][wd]} } } */
/* { dg-final { scan-assembler-times {\mrl[wd]imi} 1 } } */
