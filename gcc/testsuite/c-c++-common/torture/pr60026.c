/* PR ipa/60026 */
/* { dg-do compile } */

struct S { int f; } a;

__attribute__((optimize (0)))
struct S foo (int x, struct S y)
{
  int b = y.f;
  return a; 
}

void
bar ()
{
  while (a.f)
    {
      struct S c = {0};
      foo (0, c);
    }
}

int
main ()
{
  bar (); 
  return 0;
}
