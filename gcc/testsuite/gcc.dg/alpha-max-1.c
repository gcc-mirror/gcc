/* Test that the MAX isa builtins compile.  */
/* { dg-do link { target alpha*-*-* } } */
/* { dg-options "-mcpu=pca56" } */

void test_MAX (long x, long y)
{
  volatile long sink;

  sink = __builtin_alpha_pklb (x);
  sink = __builtin_alpha_pkwb (x);
  sink = __builtin_alpha_unpkbl (x);
  sink = __builtin_alpha_unpkbw (x);

  sink = __builtin_alpha_minub8 (0, x);
  sink = __builtin_alpha_minub8 (1, x);
  sink = __builtin_alpha_minub8 (x, y);
  sink = __builtin_alpha_minsb8 (x, y);
  sink = __builtin_alpha_minuw4 (x, y);
  sink = __builtin_alpha_minsw4 (x, y);
  sink = __builtin_alpha_maxub8 (x, y);
  sink = __builtin_alpha_maxsb8 (x, y);
  sink = __builtin_alpha_maxuw4 (x, y);
  sink = __builtin_alpha_maxsw4 (x, y);
  sink = __builtin_alpha_perr (x, y);
}

int main() { return 0; }
