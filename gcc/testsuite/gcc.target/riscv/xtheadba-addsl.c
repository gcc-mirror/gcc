/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadba" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadba" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

long
test_1 (long a, long b)
{
  /* th.addsl aX, aX, 1  */
  return a + (b << 1);
}

int
foos (short *x, int n)
{
  /* th.addsl aX, aX, 1  */
  return x[n];
}

long
test_2 (long a, long b)
{
  /* th.addsl aX, aX, 2  */
  return a + (b << 2);
}

int
fooi (int *x, int n)
{
  /* th.addsl aX, aX, 2  */
  return x[n];
}

long
test_3 (long a, long b)
{
  /* th.addsl aX, aX, 3  */
  return a + (b << 3);
}

long
fool (long *x, int n)
{
  /* th.addsl aX, aX, 2 (rv32)  */
  /* th.addsl aX, aX, 3 (rv64)  */
  return x[n];
}

/* { dg-final { scan-assembler-times "th.addsl\[ \t\]*a\[0-9\]+,a\[0-9\]+,a\[0-9\]+,1" 2 } } */

/* { dg-final { scan-assembler-times "th.addsl\[ \t\]*a\[0-9\]+,a\[0-9\]+,a\[0-9\]+,2" 3 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "th.addsl\[ \t\]*a\[0-9\]+,a\[0-9\]+,a\[0-9\]+,2" 2 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "th.addsl\[ \t\]*a\[0-9\]+,a\[0-9\]+,a\[0-9\]+,3" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "th.addsl\[ \t\]*a\[0-9\]+,a\[0-9\]+,a\[0-9\]+,3" 2 { target { rv64 } } } } */
