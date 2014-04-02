// { dg-do compile { target c++11 } }

namespace Q {
  inline namespace V1 {
    extern int i;		// { dg-message "" }
    extern int j;		// { dg-message "" }
    void f();			// { dg-message "" }
    void g();			// { dg-message "" }
  }
  inline namespace V2 {
    extern int j;		// { dg-message "" }
    void g();			// { dg-message "" }
  }
  extern int i;			// { dg-message "" }
  void f();			// { dg-message "" }
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
