/* { dg-do compile } */
/* { dg-options "-save-temps -O2" } */

/* Test that we emit a sub+subs sequence rather than mov+movk+cmp.  */

void g (void);
void
foo (int x)
{
  if (x != 0x123456)
    g ();
}

void
fool (long long x)
{
  if (x != 0x123456)
    g ();
}

/* { dg-final { scan-assembler-not "cmp\tw\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-not "cmp\tx\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-times "sub\tw\[0-9\]+.*" 1 } } */
/* { dg-final { scan-assembler-times "sub\tx\[0-9\]+.*" 1 } } */
/* { dg-final { scan-assembler-times "subs\tw\[0-9\]+.*" 1 } } */
/* { dg-final { scan-assembler-times "subs\tx\[0-9\]+.*" 1 } } */
