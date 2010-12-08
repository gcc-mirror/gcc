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
void g() {}			// { dg-message "note" }
template <int A::*>
void h() {}			// { dg-message "note" }


int main() {
  g<&A::f>();
  h<&A::i>();
  g<&B::f>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 24 }
  h<&B::j>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 26 }
  g<(void (A::*)()) &A::f>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 28 }
  h<(int A::*) &A::i>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 30 }
  g<(void (A::*)()) &B::f>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 32 }
  h<(int A::*) &B::j>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 34 }
  g<(void (A::*)()) 0>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 36 }
  h<(int A::*) 0>(); // { dg-error "" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 38 }

  return 0;
}
