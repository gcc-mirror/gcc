// Build don't link:

struct A {
  int operator ++(); // ERROR - candidates
  void operator ()(); // ERROR - candidates
  void operator delete(void*); // ERROR - candidates
};

struct B {
  int operator ++(int); // ERROR - candidates
  void operator ()(); // ERROR - candidates
  void operator delete(void*); // ERROR - candidates
  void f();
};

struct C : public A, public B {
};

void f()
{
  C c;
  C* cp;
  
  delete cp; // ERROR - ambiguous
  c(); // ERROR - ambiguous
  c++; // ERROR - ambiguous
}
  
