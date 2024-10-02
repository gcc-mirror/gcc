// { dg-do compile { target c++11 } }
// { dg-options "-fext-numeric-literals" }

//  Integer imaginary...

constexpr unsigned long long
operator ""i(unsigned long long n) // { dg-warning "1:integer suffix .i. shadowed by implementation" "" { target c++11_only } }
{ return 4 * n + 0; }

constexpr unsigned long long
operator ""I(unsigned long long n) // { dg-warning "1:integer suffix .I. shadowed by implementation" }
{ return 4 * n + 1; }

constexpr unsigned long long
operator ""j(unsigned long long n) // { dg-warning "1:integer suffix .j. shadowed by implementation" }
{ return 4 * n + 2; }

constexpr unsigned long long
operator ""J(unsigned long long n) // { dg-warning "1:integer suffix .J. shadowed by implementation" }
{ return 4 * n + 3; }

//  Floating-point imaginary...

constexpr long double
operator ""i(long double n) // { dg-warning "1:floating-point suffix .i. shadowed by implementation" "" { target c++11_only } }
{ return 4.0L * n + 0.0L; }

constexpr long double
operator ""I(long double n) // { dg-warning "1:floating-point suffix .I. shadowed by implementation" }
{ return 4.0L * n + 1.0L; }

constexpr long double
operator ""j(long double n) // { dg-warning "1:floating-point suffix .j. shadowed by implementation" }
{ return 4.0L * n + 2.0L; }

constexpr long double
operator ""J(long double n) // { dg-warning "1:floating-point suffix .J. shadowed by implementation" }
{ return 4.0L * n + 3.0L; }

//  Fixed-point...

constexpr long double
operator ""k(long double n) // { dg-warning "1:floating-point suffix .k. shadowed by implementation" }
{ return 4 * (n + 1) + 0; }

constexpr long double
operator ""K(long double n) // { dg-warning "1:floating-point suffix .K. shadowed by implementation" }
{ return 4 * (n + 1) + 1; }

constexpr long double
operator ""r(long double n) // { dg-warning "1:floating-point suffix .r. shadowed by implementation" }
{ return 4 * (n + 1) + 2; }

constexpr long double
operator ""R(long double n) // { dg-warning "1:floating-point suffix .R. shadowed by implementation" }
{ return 4 * (n + 1) + 3; }

//  Machine-defined...

constexpr long double
operator ""w(long double n) // { dg-warning "1:floating-point suffix .w. shadowed by implementation" }
{ return 4 * (n + 2) + 0; }

constexpr long double
operator ""W(long double n) // { dg-warning "1:floating-point suffix .W. shadowed by implementation" }
{ return 4 * (n + 2) + 1; }

constexpr long double
operator ""q(long double n) // { dg-warning "1:floating-point suffix .q. shadowed by implementation" }
{ return 4 * (n + 2) + 2; }

constexpr long double
operator ""Q(long double n) // { dg-warning "1:floating-point suffix .Q. shadowed by implementation" }
{ return 4 * (n + 2) + 3; }

int
main()
{
  auto ii = 1i;
  auto Ii = 1I;
  auto ji = 1j;
  auto Ji = 1J;

  auto ifp = 1.0i;
  auto Ifp = 1.0I;
  auto jfp = 1.0j;
  auto Jfp = 1.0J;

  auto kfp = 1.0k; // { dg-error "fixed-point types not supported" }
  auto Kfp = 1.0K; // { dg-error "fixed-point types not supported" }
  auto rfp = 1.0r; // { dg-error "fixed-point types not supported" }
  auto Rfp = 1.0R; // { dg-error "fixed-point types not supported" }

  auto wfp = 1.0w; // { dg-error "unsupported" "" { target { ! has_w_floating_suffix } } }
  auto Wfp = 1.0W; // { dg-error "unsupported" "" { target { ! has_w_floating_suffix } } }
  auto qfp = 1.0q; // { dg-error "unsupported" "" { target { ! has_q_floating_suffix } } }
  auto Qfp = 1.0Q; // { dg-error "unsupported" "" { target { ! has_q_floating_suffix } } }
}

// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 7 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 11 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 15 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 19 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 25 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 29 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 33 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 37 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 43 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 47 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 51 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 55 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 61 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 65 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 69 }
// { dg-warning "1:literal operator suffixes not preceded by" "" { target *-*-* } 73 }
