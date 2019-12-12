/* PR tree-optimization/32139 */
/* { dg-require-effective-target indirect_calls } */

int foo (void);
int bar (void) __attribute__ ((const));

int
test (int x)
{
  int a = (x == 10000 ? foo : bar) ();
  int b = (x == 10000 ? foo : bar) ();
  return a + b;
}
