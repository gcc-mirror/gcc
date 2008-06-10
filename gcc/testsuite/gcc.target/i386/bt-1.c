/* PR target/36473 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

extern void foo (void);

int test(int x, int n)
{
  if (x & ( 0x01 << n ))
    foo ();

  return 0;
}

/* { dg-final { scan-assembler "btl\[ \t\]" } } */
