// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

int i;

struct A
{
  ~A ();
};

A::~A () {
  i = 1;
}

struct B : virtual public A {
};

struct C {
  C ();

  B b;
};

C::C () {
  throw 3;
}

int main () 
{
  try { 
    C c;
  } catch (...) {
  }

  if (i != 1)
    return 1;
}
