/* Test that the base isa builtins compile.  */
/* { dg-do link { target alpha*-*-* } } */
/* { dg-options "-mcpu=ev4" } */

void test_BASE (long x, long y)
{
  volatile long sink;
  long z;

  sink = __builtin_alpha_implver ();
  sink = __builtin_alpha_rpcc ();

  sink = __builtin_alpha_amask (-1);
  sink = __builtin_alpha_amask (x);

  sink = __builtin_alpha_cmpbge (x, y);
  sink = __builtin_alpha_cmpbge (-1, x);
  sink = __builtin_alpha_extql (x, y);
  sink = __builtin_alpha_extqh (x, y);
}

void test_zap (long x, long y)
{
  volatile long sink;
  long z;
  sink = __builtin_alpha_zap (x, y);
  sink = __builtin_alpha_zap (x, 0xaa);
  z = 0xaa;
  sink = __builtin_alpha_zap (x, z);
  z = 0;
  sink = __builtin_alpha_zap (z, x);
  sink = __builtin_alpha_zap (x, z);
}

void test_zapnot (long x, long y)
{
  volatile long sink;
  long z;

  sink = __builtin_alpha_zapnot (x, y);
  sink = __builtin_alpha_zapnot (x, 0xaa);
  z = 0xaa;
  sink = __builtin_alpha_zapnot (x, z);
  z = 0;
  sink = __builtin_alpha_zapnot (z, x);
  sink = __builtin_alpha_zapnot (x, z);
}

int main() { return 0; }
