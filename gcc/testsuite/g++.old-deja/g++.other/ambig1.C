// Build don't link:

struct A {
  int operator ++();
  void operator ()();
  void operator delete(void*);
};

struct B {
  int operator ++(int);
  void operator ()();
  void operator delete(void*);
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
  
