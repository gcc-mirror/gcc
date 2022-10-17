/* PR middle-end/55881 - #pragma GCC diagnostic ignored ignored when inlining
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct I
{
  int i;
  int foo (struct I *n)
  {
    return n->i + 10;         // { dg-bogus "-Wmaybe-uninitialized" }
  }

  I () : i (5) {}
};

int main (int argc, char **)
{
  struct I i, *n;

  if (argc > 10)
    {
      n = new I;
      i.i = i.foo (n);
  }

  if (argc > 2)
    {
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
      return i.foo (n);
#pragma GCC diagnostic pop
    }

  return 0;
}
