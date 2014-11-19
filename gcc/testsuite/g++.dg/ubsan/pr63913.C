// PR sanitizer/63913
// { dg-do compile }
// { dg-options "-fsanitize=bool -fnon-call-exceptions" }

struct B { B (); ~B (); };

double
foo (bool *x)
{
  B b;
  return *x;
}
