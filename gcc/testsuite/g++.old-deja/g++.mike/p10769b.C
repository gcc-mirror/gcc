// { dg-do assemble  }
// { dg-options "" }
// prms-id: 10769

#define PMF2PF(PMF) ((void (*)())(PMF))

class A {
public:
  void f1a() { }
  void main();
} a;

class B {
public:
  void bf1() { }
} b;

void A::main() {
  void (B::*mPtrB)(B*);
  (*(void (*)(A*))PMF2PF(mPtrB))(&b);	// { dg-error "argument passing" } 
  // { dg-warning "convert" "warn" { target *-*-* } 20 }
}

int main() {
  void (A::*mPtr)() = &A::f1a;
  (*(void (*)(A*))PMF2PF(mPtr))(&a);	// { dg-warning "convert" } 
}
