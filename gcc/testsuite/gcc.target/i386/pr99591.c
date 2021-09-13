/* PR tree-optimization/99591 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tmovs\[bw]l\t" } } */

int
foo (signed char a, signed char b)
{
  signed char r;
  return __builtin_add_overflow (a, b, &r);
}

int
bar (short a, short b)
{
  short r;
  return __builtin_add_overflow (a, b, &r);
}

int
baz (signed char a, signed char b)
{
  signed char r;
  return __builtin_add_overflow ((int) a, (int) b, &r);
}

int
qux (short a, short b)
{
  short r;
  return __builtin_add_overflow ((int) a, (int) b, &r);
}
