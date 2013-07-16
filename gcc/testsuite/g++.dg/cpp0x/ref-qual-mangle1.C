// PR c++/56821
// { dg-require-effective-target c++11 }

struct A {
  // { dg-final { scan-assembler "_ZNR1A1fEv" } }
  void f() & {}
  // { dg-final { scan-assembler "_ZNO1A1gEv" } }
  void g() && {}
  // { dg-final { scan-assembler "_ZNKR1A1hEv" } }
  void h() const & {}
};

// { dg-final { scan-assembler "_Z1jM1AFvvRE" } }
void j(void (A::*)() &) { }
// { dg-final { scan-assembler "_Z1kM1AFvvOE" } }
void k(void (A::*)() &&) { }
// { dg-final { scan-assembler "_Z1lM1AKFvvRE" } }
void l(void (A::*)() const &) { }

// { dg-final { scan-assembler "_Z1mIFvvOEEvM1AT_" } }
// { dg-final { scan-assembler "_Z1mIFvvREEvM1AT_" } }
// { dg-final { scan-assembler "_Z1mIKFvvREEvM1AT_" } }
template <typename T>
void m(T A::*) {}

// { dg-final { scan-assembler "_Z1nIM1AFvvOEEvT_" } }
// { dg-final { scan-assembler "_Z1nIM1AFvvREEvT_" } }
// { dg-final { scan-assembler "_Z1nIM1AKFvvREEvT_" } }
template <typename T>
void n(T) {}

int main()
{
  j(&A::f); k(&A::g); l(&A::h);
  m(&A::f); m(&A::g); m(&A::h);
  n(&A::f); n(&A::g); n(&A::h);
}
