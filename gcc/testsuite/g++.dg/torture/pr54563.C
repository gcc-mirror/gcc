// PR tree-optimization/54563
// { dg-do compile }

extern "C" float powf (float, float);
struct S { ~S (); };
double bar ();
double x;

void
foo ()
{
  S s;
  x = powf (bar (), 2.);
}
