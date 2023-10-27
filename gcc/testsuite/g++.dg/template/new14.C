// Verify we check new-expressions ahead of time.

struct A { };
struct B { B(int); };
struct C { void* operator new(__SIZE_TYPE__, int); };

template<class T>
void f() {
  new A(1); // { dg-error "no match" "" { xfail *-*-* } }
  new B(1, 2); // { dg-error "no match" }
  new B; // { dg-error "no match" }
  new C; // { dg-error "no match" }
}


template<class T>
void g() {
  new int[__SIZE_MAX__]; // { dg-error "exceeds maximum" }
  new int[__SIZE_MAX__ / sizeof(int)]; // { dg-error "exceeds maximum" }
}
