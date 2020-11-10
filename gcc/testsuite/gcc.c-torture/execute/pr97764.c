/* PR tree-optimization/97764 */
/* { dg-require-effective-target int32plus } */

struct S { int b : 3; int c : 28; int d : 1; };

int
main ()
{
  struct S e = {};
  e.c = -1;
  if (e.d)
    __builtin_abort ();
  return 0;
}
