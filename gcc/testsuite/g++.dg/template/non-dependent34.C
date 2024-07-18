// Verify we diagnose failed qualified lookup into the current
// instantiation ahead of time.

namespace without_dependent_base {
template<class T>
struct A {
  void f(A& other) {
    A::x; // { dg-error "'x' is not a member" }
    this->x; // { dg-error "no member named 'x'" }
    other.y; // { dg-error "no member named 'y'" }
    typename A::type z; // { dg-error "does not name a type" }

    struct B {
      void g(A& other) {
	A::x; // { dg-error "'x' is not a member" }
	this->x; // { dg-error "no member named 'x'" }
	other.y; // { dg-error "no member named 'y'" }
	typename A::type z; // { dg-error "does not name a type" }
      }
    };
  }
};
}

namespace with_dependent_base {
template<class T>
struct A : T {
  void f(A& other) {
    A::x;
    this->x;
    other.y;
    typename A::type z;

    struct B : T {
      void g(A& other) {
	A::x;
	this->x;
	other.y;
	typename A::type z;
      }
    };
  }
};
}
