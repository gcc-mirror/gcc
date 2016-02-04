/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mtune=core2" } */

extern void foo (void);

int test (long long x)
{
  if (x & ( 0x01ULL << 60 ))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler "btq\[ \t\]" } } */
