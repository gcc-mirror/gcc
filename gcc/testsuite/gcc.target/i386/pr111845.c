/* PR tree-optimization/111845 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -masm=att" } */
/* { dg-final { scan-assembler-times "\tadcq\t" 8 { target lp64 } } } */
/* { dg-final { scan-assembler-times "\tadcl\t" 8 { target ia32 } } } */

unsigned long l, m;

__attribute__((noipa)) void
foo (unsigned long x, unsigned long y, unsigned long h, unsigned long i, int a, int b)
{
  unsigned long c, d;
  unsigned long e = __builtin_add_overflow (x, y, &c);
  unsigned long f = __builtin_add_overflow (c, a < b, &d);
  m = ((h + i) + e) + f;
  l = d;
}

__attribute__((noipa)) void
bar (unsigned long x, unsigned long y, unsigned long h, unsigned long i, int a, int b)
{
  unsigned long c, d;
  unsigned long e = __builtin_add_overflow (x, y, &c);
  unsigned long f = __builtin_add_overflow (c, a < b, &d);
  m = (h + (i + e)) + f;
  l = d;
}

__attribute__((noipa)) void
baz (unsigned long x, unsigned long y, unsigned long h, unsigned long i, int a, int b)
{
  unsigned long c, d;
  unsigned long e = __builtin_add_overflow (x, y, &c);
  unsigned long f = __builtin_add_overflow (c, a < b, &d);
  m = h + (i + (e + f));
  l = d;
}

__attribute__((noipa)) void
qux (unsigned long x, unsigned long y, unsigned long h, unsigned long i, int a, int b)
{
  unsigned long c, d;
  unsigned long e = __builtin_add_overflow (x, y, &c);
  unsigned long f = __builtin_add_overflow (c, a < b, &d);
  m = h + ((i + e) + f);
  l = d;
}
