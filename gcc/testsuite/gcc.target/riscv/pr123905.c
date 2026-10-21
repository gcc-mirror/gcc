/* PR target/123905 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcbv -mabi=lp64d" } */
/* { dg-options "-O2 -march=rv32gcbv -mabi=ilp32" } */

int
foo1 (unsigned a, unsigned b)
{
  return __builtin_clz (a) & ~b;
}

int
foo2 (unsigned a, unsigned b)
{
  return __builtin_ctz (a) & ~b;
}

int
foo3 (unsigned a, unsigned b)
{
  return __builtin_popcount (a) & ~b;
}

int
test_and (unsigned a, unsigned b)
{
  return __builtin_clz (a) & b;
}

int
test_and2 (unsigned a, unsigned b)
{
  return __builtin_ctz (a) & b;
}

int
test_and3 (unsigned a, unsigned b)
{
  return __builtin_popcount (a) & b;
}

int
test_and_and (unsigned a, unsigned c)
{
  return __builtin_clz (a) & __builtin_ctz (c);
}

int
test_and_and_and (unsigned a, unsigned b, unsigned c)
{
  return __builtin_clz (a) & __builtin_ctz (b) & __builtin_popcount (c);
}

/* { dg-final { scan-assembler-not "sext\\.w" } } */
/* { dg-final { scan-assembler-not "andi\\s" } } */
