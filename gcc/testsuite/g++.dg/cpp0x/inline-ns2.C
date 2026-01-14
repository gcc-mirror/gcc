// { dg-do compile { target c++11 } }

namespace Q {
  inline namespace V1 {
    extern int i;		// { dg-message "candidate" }
    extern int j;		// { dg-message "candidate" }
    void f();			// { dg-message "candidate" }
    void g();			// { dg-message "candidate" }
  }
  inline namespace V2 {
    extern int j;		// { dg-message "candidate" }
    void g();			// { dg-message "candidate" }
  }
  extern int i;			// { dg-message "candidate" }
  void f();			// { dg-message "candidate" }
  void h();
}
namespace R {
  using namespace Q;
}
int Q::i = 1;			// { dg-error "ambiguous" }
int Q::j = 1;			// { dg-error "ambiguous" }
void Q::f() { }			// { dg-error "ambiguous" }
void Q::g() { }			// { dg-error "ambiguous" }
void R::h() { }			// { dg-error "should have been declared inside 'R'" }
