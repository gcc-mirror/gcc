// PR middle-end/104679
// { dg-do compile }

struct A { ~A (); };
void foo (double, long);
void bar ();
double a;
long b;

void
baz ()
{
  foo (a, b);
  if (a == 0.0)
    ;
  else
    while (a > 0.0)
      {
        A c;
        bar ();
      }
}
