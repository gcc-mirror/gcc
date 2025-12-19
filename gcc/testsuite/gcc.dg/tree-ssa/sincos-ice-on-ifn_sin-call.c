/* { dg-do compile } */
/* { dg-additional-options "-O -fgimple" } */
typedef float V __attribute__ ((vector_size (16)));

void __GIMPLE (ssa, startwith ("sincos"))
foo (float * output, const float * input)
{
  V a;
  V b;
  V c;
  V d;

  __BB(2):
  a = __MEM <const V> (input);
  b = .COS (a);
  c = .SIN (a);
  d = a + b;
  return;
}
