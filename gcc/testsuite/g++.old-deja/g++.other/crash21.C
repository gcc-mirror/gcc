// Build don't link:
// Origin: Gabriel Dos Reis <gdr@codesourcery.com>

struct  A {
  virtual void f(int&) const;
};

struct B : public A {
  int x;
};

void B::f(int& t) { // ERROR - undeclared method
  x = t;
}
