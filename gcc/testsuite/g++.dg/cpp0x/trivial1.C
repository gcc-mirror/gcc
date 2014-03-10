// { dg-do compile { target c++11 } }

// [basic.types]/10:
// Scalar types, trivial class types (Clause 9), arrays of such types and
// cv-qualified versions of these types (3.9.3) are collectively called
// trivial types.

// [class]/6:
// A trivially copyable class is a class that:
// * has no non-trivial copy constructors (12.8),
// * has no non-trivial copy assignment operators (13.5.3, 12.8), and
// * has a trivial destructor (12.4).
// A trivial class is a class that has a trivial default constructor (12.1)
// and is trivially copyable.

#include <type_traits>

#define TRY(expr) static_assert (expr, #expr)
#define YES(type) TRY(std::is_trivial<type>::value); \
  TRY(std::is_trivial<type[]>::value); \
  TRY(std::is_trivial<const volatile type>::value)
#define NO(type) TRY(!std::is_trivial<type>::value); \
  TRY(!std::is_trivial<type[]>::value); \
  TRY(!std::is_trivial<const volatile type>::value)

struct A;

YES(int);
YES(__complex int);
YES(void *);
YES(int A::*);
typedef int (A::*pmf)();
YES(pmf);

struct A { ~A(); };
NO(A);
struct F: public A { int i; };
NO(F);
struct G: public A { A a; };
NO(G);
struct M { A a; };
NO(M);

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
YES(E);
struct D2: public B { };
YES(D2);
struct I: public D, public D2 { };
YES(I);

struct C
{
  int i;
private:
  int j;
};
YES(C);
struct H: public C { };
YES(H);
struct N { C c; };
YES(N);

struct J { virtual void f(); };
struct J2: J { };
NO(J);
NO(J2);
struct K { };
struct L: virtual K {};
YES(K);
NO(L);

// PR c++/41421
struct O { O(int); };
NO(O);
