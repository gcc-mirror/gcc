/* Test that the base isa builtins compile.  */
/* { dg-do link } */
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

  sink = __builtin_alpha_extbl (x, y);
  sink = __builtin_alpha_extwl (x, y);
  sink = __builtin_alpha_extll (x, y);
  sink = __builtin_alpha_extql (x, y);
  sink = __builtin_alpha_extwh (x, y);
  sink = __builtin_alpha_extlh (x, y);
  sink = __builtin_alpha_extqh (x, y);

  sink = __builtin_alpha_insbl (x, y);
  sink = __builtin_alpha_inswl (x, y);
  sink = __builtin_alpha_insll (x, y);
  sink = __builtin_alpha_insql (x, y);
  sink = __builtin_alpha_inswh (x, y);
  sink = __builtin_alpha_inslh (x, y);
  sink = __builtin_alpha_insqh (x, y);

  sink = __builtin_alpha_mskbl (x, y);
  sink = __builtin_alpha_mskwl (x, y);
  sink = __builtin_alpha_mskll (x, y);
  sink = __builtin_alpha_mskql (x, y);
  sink = __builtin_alpha_mskwh (x, y);
  sink = __builtin_alpha_msklh (x, y);
  sink = __builtin_alpha_mskqh (x, y);

  sink = __builtin_alpha_umulh (x, y);
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
