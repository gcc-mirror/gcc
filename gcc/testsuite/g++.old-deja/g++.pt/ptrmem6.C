// { dg-do assemble  }

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
  g<&B::f>(); // { dg-error "" } 
  h<&B::j>(); // { dg-error "" } 
  g<(void (A::*)()) &A::f>(); // { dg-error "" } 
  h<(int A::*) &A::i>(); // { dg-error "" } 
  g<(void (A::*)()) &B::f>(); // { dg-error "" } 
  h<(int A::*) &B::j>(); // { dg-error "" } 
  g<(void (A::*)()) 0>(); // { dg-error "" } 
  h<(int A::*) 0>(); // { dg-error "" } 

  return 0;
}
