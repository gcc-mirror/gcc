// P2468R2 - The Equality Operator You Are Looking For
// { dg-do compile { target c++20 } }

struct A {
  bool operator== (const A &) { return true; }
  bool operator!= (const A &) { return false; }
};
bool a = A{} != A{};

template <typename T>
struct B {
  bool operator== (const T &) const;
  bool operator!= (const T &) const;
};
struct B1 : B<B1> { };
bool b1 = B1{} == B1{};
bool b2 = B1{} != B1{};

template <bool>
struct C {
  using C1 = C<true>;
  using C2 = C<false>;
  C () = default;
  C (const C2 &);
  bool operator== (C1) const;
  bool operator!= (C1) const;
};
using C3 = C<false>;
bool c = C3{} == C3{};

struct D {
  D ();
  D (int *);
  bool operator== (const D &) const;	// { dg-message "candidate 1: 'bool D::operator==\\\(const D&\\\) const' \\\(reversed\\\)" }
  operator int * () const;
};
bool d = nullptr != D{};	// { dg-error "ambiguous overload for 'operator!=' in 'nullptr != D\\\(\\\)' \\\(operand types are 'std::nullptr_t' and 'D'\\\)" }
				// { dg-message "candidate 2: 'operator!=\\\(int\\\*, int\\\*\\\)' \\\(built-in\\\)" "" { target *-*-* } .-1 }

using ubool = unsigned char;

struct E {
  operator bool () const;
};
unsigned char operator== (E, E);// { dg-message "candidate 2: 'unsigned char operator==\\\(E, E\\\)'" }
				// { dg-message "no known conversion for argument 1 from 'int' to 'E'" "" { target *-*-* } .-1 }
unsigned char e = E{} != E{};	// { dg-error "return type of 'unsigned char operator==\\\(E, E\\\)' is not 'bool'" }
				// { dg-message "used as rewritten candidate for comparison of 'E' and 'E'" "" { target *-*-* } .-1 }

// F-H are the testcase from [over.match.oper]
struct F {};
template <typename T>
bool operator== (F, T);		// { dg-message "candidate 1: 'template<class T> bool operator==\\\(F, T\\\)'" }
				// { dg-message "template argument deduction/substitution failed:" "" { target *-*-* } .-1 }
bool f1 = 0 == F ();		// OK, calls reversed ==
template <typename T>
bool operator!= (F, T);
bool f2 = 0 == F ();		// { dg-error "no match for 'operator==' in '0 == F\\\(\\\)' \\\(operand types are 'int' and 'F'\\\)" }
				// { dg-message "cannot convert '0' \\\(type 'int'\\\) to type 'F'" "" { target *-*-* } .-1 }

struct G {
  bool operator== (const G &);
};
struct G1 : G {
  G1 ();
  G1 (G);
  bool operator!= (const G &);
};
bool g1 = G () == G1 ();	// OK, != prevents rewrite
bool g2 = G1 () == G ();	// { dg-error "ambiguous, even though the second is reversed" }

struct H {};
template <typename T>
bool operator== (H, T);
inline namespace H1 {
  template <typename T>
  bool operator!= (H, T);
}
// [over.match.oper] currently says that this is OK because the inline
// namespace isn't searched, but that seems wrong to me, so I'm going to go
// ahead and search it for now.  Remove the "0 &&" in add_candidates to
// change this to the currently specified behavior.
// { dg-error "no match" "" { target *-*-* } .+1 }
bool h = 0 == H ();		// OK, calls reversed ==

template <class T>
struct I {
  int operator== (const double &) const;
  friend inline int operator== (const double &, const T &) { return 1; }
};
struct I1 : I<I1> { };
bool i = I1{} == 0.;		// { dg-error "return type of 'int operator==\\\(const double&, const I1&\\\)' is not 'bool'" }
				// { dg-message "used as rewritten candidate for comparison of 'I1' and 'double'" "" { target *-*-* } .-1 }

struct J {
  bool operator== (const J &) const;
  bool operator!= (const J &) const;
};
struct J1 : J {
  J1 (const J &);
  bool operator== (const J1 &x) const {
    return static_cast<const J &> (*this) == x;	// { dg-error "ambiguous overload for 'operator==' in '\\\*\\\(const J\\\*\\\)\\\(\\\(const J1\\\*\\\)this\\\) == x' \\\(operand types are 'const J' and 'const J1'\\\)" }
  }
};

struct K {
  bool operator== (const K &);
};
bool k = K{} == K{};		// { dg-error "ambiguous, even though the second is reversed" }

struct L {
  bool operator== (const L &) const;
};
bool l = L{} == L{};

struct M {
  bool operator== (M);
};
bool m = M () == M ();

struct N {
  virtual bool operator== (const N &) const;
};
struct N1 : N {
  bool operator== (const N &) const override;
};
bool n = N1 () == N1 ();	// { dg-error "ambiguous, even though the second is reversed" }

struct O {
  virtual signed char operator== (const O &) const;
  signed char operator!= (const O &x) const { return !operator== (x); }
};
struct O1 : O {
  signed char operator== (const O &) const override;
};
bool o = O1 () != O1 ();

template <class T>
bool
foo (T x, T y)
requires requires { x == y; }
{
  return x == y;
}
bool b3 = foo (B1 (), B1 ());

struct P {};
template <typename T, class U = int>
bool operator== (P, T);
template <class T>
bool operator!= (P, T);
bool p = 0 == P ();

struct Q {};
template <typename T>
bool operator== (Q, T);
template <typename U>
bool operator!= (Q, U);
bool q = 0 == Q ();		// { dg-error "" }

struct R {
  template <typename T>
  bool operator== (const T &);
};
bool r = R () == R ();		// { dg-error "ambiguous, even though the second is reversed" }

struct S {
  template <typename T>
  bool operator== (const T &) const;
  bool operator!= (const S &);
};
bool s = S () == S ();

struct T {};
template <class U = int>
bool operator== (T, int);
bool operator!= (T, int);
bool t = 0 == T ();

struct U {};
bool operator== (U, int);
bool u1 = 0 == U ();
namespace U1 { bool operator!= (U, int); }
bool u2 = 0 == U ();
using U1::operator!=;
bool u3 = 0 == U ();		// { dg-error "" }

struct V {};
template <typename T>
bool operator== (V, T);
bool v1 = 0 == V ();
namespace V1 { template <typename T> bool operator!= (V, T); }
bool v2 = 0 == V ();
using V1::operator!=;
bool v3 = 0 == V ();		// { dg-error "" }

template <int N>
struct W {
bool operator== (int) requires (N == 1);
bool operator!= (int) requires (N == 2);
};
int w = 0 == W<1> ();

struct X {
  bool operator== (X const &);
  static bool operator!= (X const &, X const &);	// { dg-error "'static bool X::operator!=\\\(const X&, const X&\\\)' must be either a non-static member function or a non-member function" }
};
bool x = X () == X ();		// { dg-error "ambiguous, even though the second is reversed" }
