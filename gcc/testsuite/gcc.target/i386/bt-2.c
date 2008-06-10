/* PR target/36473 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

extern void foo (void);

int test(long x, long n)
{
  if (x & ( (long)0x01 << n ))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler "btl\[ \t\]" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "btq\[ \t\]" { target lp64 } } } */
