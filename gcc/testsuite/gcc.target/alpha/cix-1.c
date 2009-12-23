/* Test that the CIX isa builtins compile.  */
/* { dg-do link } */
/* { dg-options "-mcpu=ev67" } */

void test_CIX (long x)
{
  volatile long sink;

  sink = __builtin_alpha_cttz (x);
  sink = __builtin_alpha_ctlz (x);
  sink = __builtin_alpha_ctpop (x);
}

int main() { return 0; }
