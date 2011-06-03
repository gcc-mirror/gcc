// PR c++/29131
// int has no associated namespaces, so arg-dep lookup doesn't find g(int).

template <class T> int f() { return g(T()); } // { dg-error "argument-dependent" }
int g(int);				      // { dg-message "declared here" }
int i = f<int>();

// PR c++/24163
// Unqualified lookup doesn't find names from dependent bases.

template <class T>
struct A
{
  static void h(T);
};

template <class T> struct B: A<T>
{
  void f() { h(T()); }		// { dg-error "argument-dependent" }
  static void g() { h(T()); }	// { dg-error "argument-dependent" }
};

int main()
{
  B<int> b;
  b.f();
  b.g();
}

// { dg-message "dependent base .A.int" "" { target *-*-* } 19 }
// { dg-message "this->h" "" { target *-*-* } 19 }
// { dg-message "dependent base .A.int" "" { target *-*-* } 20 }
// { dg-message "B::h" "" { target *-*-* } 20 }
