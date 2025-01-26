/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mno-lsx -std=gnu11" } */

extern char a[64];
extern char b[64];

#ifndef TEST_TARGET_PRAGMA
__attribute__ ((target ("arch=la64v1.1")))
#else
#pragma GCC target ("arch=la64v1.1")
#endif
void
test (void)
{
  for (int i = 0; i < 64; i++)
    a[i] = b[i];
}


/* { dg-final { scan-assembler "vld" } } */
