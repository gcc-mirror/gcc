// Test for PMF template args.
// { dg-options -std=c++1z }
// { dg-do compile }

struct A
{
  void f() noexcept;
  void g();
};

template <void (A::*)()> struct B { };
template <void (A::*)() noexcept> struct C { };

B<&A::f> b1;
B<&A::g> b2;

C<&A::f> c1;
C<&A::g> c2;			// { dg-error "" }
