// Bug: the temporary returned from f is elided, causing a to be constructed
// twice but only destroyed once.

extern "C" int printf (const char *, ...);

int c,d;

struct A {
  A (int) { c++; }
  ~A () { d++; }
  A (const A&) { c++; }
  int i;
};

A f ()
{ return 1; }

int main ()
{
  {
    A a (1);
    a = f ();
  }
  printf ("%d %d\n", c, d);
  return c != d;
}
