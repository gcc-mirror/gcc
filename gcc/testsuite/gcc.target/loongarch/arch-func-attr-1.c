/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mno-lsx" } */

extern char a[64];
extern char b[64];

__attribute__ ((target ("arch=la64v1.1")))
void
test (void)
{
  for (int i = 0; i < 64; i++)
    a[i] = b[i];
}


/* { dg-final { scan-assembler "vld" } } */
