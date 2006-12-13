// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

struct S { ~S () throw () {} };
double bar ();

int
foo ()
{
  S a;
  int i = 0;
  double c = bar ();
  c = c < 0 ? -c : c;
  if (c <= 1.e-8)
    i += 24;
  return i;
}
