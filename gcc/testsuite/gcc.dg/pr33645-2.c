/* PR tree-optimization/33645 */
/* { dg-do compile } */
/* { dg-options "-O2 -funit-at-a-time" } */

__attribute__((noinline)) int
bar (int *x)
{
  return *x++;
}

int
main ()
{
  static int var1_s;
  static int *var1_t = &var1_s;

  return bar (var1_t) != 0;
}

/* { dg-final { scan-assembler-not "var1_t" } } */
