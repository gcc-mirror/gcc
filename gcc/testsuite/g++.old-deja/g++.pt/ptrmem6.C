// Build don't link:

class A {
public:
  virtual void f();
  int i;
};

class B : public A {
public:
  void f();
  int j;
};

template <void (A::*)() >
void g() {}
template <int A::*>
void h() {}


int main() {
  g<&A::f>();
  h<&A::i>();
  g<&B::f>(); // ERROR - 
  h<&B::j>(); // ERROR - 
  g<(void (A::*)()) &A::f>(); // ERROR - XFAIL *-*-*
  h<(int A::*) &A::i>(); // ERROR - 
  g<(void (A::*)()) &B::f>(); // ERROR - 
  h<(int A::*) &B::j>(); // ERROR - 
  g<(void (A::*)()) 0>(); // ERROR - 
  h<(int A::*) 0>(); // ERROR - 

  return 0;
}
