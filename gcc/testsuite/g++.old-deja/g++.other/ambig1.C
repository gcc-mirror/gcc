// { dg-do assemble  }

struct A {
  int operator ++(); // { dg-error "" } candidates
  void operator ()(); // { dg-error "" } candidates
  void operator delete(void*); // { dg-error "" } candidates
};

struct B {
  int operator ++(int); // { dg-error "" } candidates
  void operator ()(); // { dg-error "" } candidates
  void operator delete(void*); // { dg-error "" } candidates
  void f();
};

struct C : public A, public B {
};

void f()
{
  C c;
  C* cp;
  
  delete cp; // { dg-error "" } ambiguous
  c(); // { dg-error "" } ambiguous
  c++; // { dg-error "" } ambiguous
}
  
