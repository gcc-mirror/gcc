/* PR tree-optimization/82388 */

struct A { int b; int c; int d; } e;

struct A
foo (void)
{
  struct A h[30] = {{0,0,0}};
  return h[29]; 
}

int
main ()
{
  e = foo ();
  return e.b; 
}
