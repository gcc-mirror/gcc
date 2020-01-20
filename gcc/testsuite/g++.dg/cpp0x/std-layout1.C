// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-deprecated-declarations" { target c++2a } }

// [basic.types]/10:
// Scalar types, standard-layout class types (Clause 9), arrays of such
// types and cv-qualified versions of these types (3.9.3) are collectively
// called standard-layout types.

// [class]/7:
// A standard-layout class is a class that:
// * has no non-static data members of type non-standard-layout class (or
// array of such types) or reference,
// * has no virtual functions (10.3) and no virtual base classes (10.1),
// * has the same access control (Clause 11) for all non-static data members,
// * has no non-standard-layout base classes,
// * either has no non-static data members in the most-derived class and at
// most one base class with non-static data members, or has no base classes
// with non-static data members, and
// * has no base classes of the same type as the first non-static data member.

#include <type_traits>

#define TRY(expr) static_assert (expr, #expr)
#define YES(type) TRY(std::is_standard_layout<type>::value); \
  TRY(std::is_standard_layout<type[]>::value); \
  TRY(std::is_standard_layout<const volatile type>::value)
#define NO(type) TRY(!std::is_standard_layout<type>::value); \
  TRY(!std::is_standard_layout<type[]>::value); \
  TRY(!std::is_standard_layout<const volatile type>::value)
#define NONPOD(type) TRY(!std::is_pod<type>::value); \
  TRY(!std::is_pod<type[]>::value); \
  TRY(!std::is_pod<const volatile type>::value)

struct A;

YES(int);
YES(__complex int);
YES(void *);
YES(int A::*);
typedef int (A::*pmf)();
YES(pmf);

struct A { ~A(); };
YES(A);
NONPOD(A);
struct F: public A { int i; };
YES(F);
NONPOD(F);
struct G: public A { A a; };
NO(G);
struct M { A a; };
YES(M);

class B
{
  int i;
  __complex int c;
  void *p;
  double ar[4];
  int A::* pm;
  int (A::*pmf)();
};
YES(B);
struct D: public B { };
YES(D);
struct E: public B { int q; };
NO(E);
struct D2: public B { };
YES(D2);
struct I: public D, public D2 { };
NO(I);

struct C
{
  int i;
private:
  int j;
};
NO(C);
struct H: public C { };
NO(H);
struct N { C c; };
NO(N);

struct J { virtual void f(); };
struct J2: J { };
NO(J);
NO(J2);
struct K { };
struct L: virtual K {};
YES(K);
NO(L);
