// PR c++/411

// Test that a fully-constructed base is destroyed before transferring
// control to the handler of a function-try-block.

// { dg-do run { xfail *-*-* } }

int ad;
int r;

struct A {
  ~A() { ++ad; }
};

struct B: public A {
  ~B();
};

B::~B ()
try
  {
    throw 1;
  }
catch (...)
  {
    if (!ad)
      r = 1;
    return;
  }

int main ()
{
  { B b; }
  return r;
}
