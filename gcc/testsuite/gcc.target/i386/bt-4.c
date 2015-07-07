/* { dg-do compile } */
/* { dg-options "-Os -mtune=core2" } */

extern void foo (void);

int test (long x)
{
  if (x & ( 0x01UL << 10 ))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler "btl\[ \t\]" } } */
