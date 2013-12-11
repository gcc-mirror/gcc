/* PR tree-optimization/59386 */

struct S { int s; };
struct T { int t; struct S u; } c;
int b;

struct S
foo ()
{
  struct T d;
  if (b)
    while (c.t)
      ;
  else
    return d.u;
}

struct S
bar ()
{
  struct T a;
  a.u = foo ();
  return a.u;
}
