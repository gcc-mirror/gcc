// { dg-do assemble  }

struct A {
  int operator ++(); // { dg-message "" } candidates
  void operator ()(); // { dg-message "" } candidates
  void operator delete(void*); // { dg-message "" } candidates
};

struct B {
  int operator ++(int); // { dg-message "" } candidates
  void operator ()(); // { dg-message "" } candidates
  void operator delete(void*); // { dg-message "" } candidates
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
  
