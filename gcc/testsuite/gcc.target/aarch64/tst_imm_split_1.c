/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f (unsigned char *p)
{
  return p[0] == 50 || p[0] == 52;
}

int
g (unsigned char *p)
{
  return (p[0] >> 4 & 0xfd) == 0;
}

/* { dg-final { scan-assembler-not "and\\t\[xw\]\[0-9\]+, \[xw\]\[0-9\]+.*" } } */
/* { dg-final { scan-assembler-times "tst\\t\[xw\]\[0-9\]+, \[xw\]\[0-9\]+" 2 } } */
