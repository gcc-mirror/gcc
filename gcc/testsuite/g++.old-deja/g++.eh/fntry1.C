// { dg-do run  }
// Bug: g++ fails to treat function-try-blocks in ctors specially.
// Submitted by Jason Merrill <jason@cygnus.com>

int c;
int r;

struct A {
  int i;
  A(int j) { i = j; }
  ~A() { c += i; }
};

struct B: public A {
  A a;
  B() try : A(1), a(2)
    { throw 1; }
  catch (...)
    { if (c != 3) r |= 1; }
};

int main ()
{
  try
    { B b; }
  catch (...)
    { c = 0; }

  if (c != 0) r |= 2;

  return r;
}
