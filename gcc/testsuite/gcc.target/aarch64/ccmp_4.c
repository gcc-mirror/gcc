/* { dg-options "-O2" } */
/* PR target/100942 */

void foo(void);
int f1(int a, int b, int d)
{
  int c = a < 8 || b < 9;
  int e = d < 11 || c;
  if (e) foo();
  return c;
}

/*
  We really should get:
        cmp     w0, 7
        ccmp    w1, 8, 4, gt
        cset    w0, le
        ccmp    w2, 10, 4, gt
        ble     .L11

  But we currently get:
        cmp     w0, 7
        ccmp    w1, 8, 4, gt
        cset    w0, le
        cmp     w0, 0
        ccmp    w2, 10, 4, eq
        ble     .L11
  The middle cmp is not needed.
 */

/* We should end up with only one cmp and 2 ccmp and 1 cset but currently we get 2 cmp
   though. */
/* { dg-final { scan-assembler-times "\tccmp\t" 2 } } */
/* { dg-final { scan-assembler-times "\tcset\t" 1 } } */
/* { dg-final { scan-assembler-times "\tcmp\t" 1 { xfail *-*-* } } } */
