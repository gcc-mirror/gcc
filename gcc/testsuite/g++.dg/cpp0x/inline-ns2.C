// { dg-options -std=c++0x }

namespace Q {
  inline namespace V1 {
    extern int i;		// { dg-error "" }
    extern int j;		// { dg-error "" }
    void f();			// { dg-error "" }
    void g();			// { dg-error "" }
  }
  inline namespace V2 {
    extern int j;		// { dg-error "" }
    void g();			// { dg-error "" }
  }
  extern int i;			// { dg-error "" }
  void f();			// { dg-error "" }
  void h();
}
namespace R {
  using namespace Q;
}
int Q::i = 1;			// { dg-error "ambiguous" }
int Q::j = 1;			// { dg-error "ambiguous" }
void Q::f() { }			// { dg-error "ambiguous" }
void Q::g() { }			// { dg-error "ambiguous" }
void R::h() { }			// { dg-error "" }
