/* PR tree-optimization/103417 */
/* { dg-require-effective-target int32plus } */

struct { int a : 8; int b : 24; } c = { 0, 1 };

int
main ()
{
  if (c.b && !c.b)
    __builtin_abort ();
  return 0;
}
