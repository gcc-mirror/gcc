// PR c++/66477
// { dg-do compile { target c++11 } }

struct a { constexpr bool g() const { return true; } };
constexpr bool g(a&) { return true;}
constexpr bool h(a) { return true;}

a a1;
a& ar = a1;

void f(a ap, a& arp)
{
  a a2;
  a& ar2 = a2;

  // Most of these are OK because no data is actually loaded.
  static_assert (a1.g(),"");
  static_assert (g(a1),"");
  static_assert (h(a1),"");

  static_assert (a2.g(),"");
  static_assert (g(a2),"");
  static_assert (h(a2),"");

  static_assert (ap.g(),"");
  static_assert (g(ap),"");
  static_assert (h(ap),"");

  static_assert (ar.g(),"");
  static_assert (g(ar),"");
  static_assert (h(ar),"");

  // But these are specifically prohibited in [expr.const]/4.12:
  // * an id-expression that refers to a variable or data member of reference
  //   type unless the reference has a preceding initialization and either
  // ** it is usable in constant expressions or
  // ** its lifetime began within the evaluation of e;

  static_assert (ar2.g(),"");	// { dg-error "constant" }
  static_assert (g(ar2),"");	// { dg-error "constant" }
  static_assert (h(ar2),"");	// { dg-error "constant" }

  static_assert (arp.g(),"");	// { dg-error "constant" }
  static_assert (g(arp),"");	// { dg-error "constant" }
  static_assert (h(arp),"");	// { dg-error "constant" }
}
