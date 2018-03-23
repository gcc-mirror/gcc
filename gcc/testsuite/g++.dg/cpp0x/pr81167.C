// PR c++/81167
// { dg-do compile { target c++11 } }
// { dg-options "-Wconversion" }

struct bar;

struct foo
{
  foo () {}
  foo (const bar &) {}
};

struct bar
{
  operator foo () && { return foo (); }
};

void test ()
{
  foo f = bar ();
// { dg-warning "choosing 'bar::operator foo\\(\\) &&' over 'foo::foo\\(const bar&\\)'" "" { target *-*-* } .-1 }
// { dg-warning "for conversion from 'bar' to 'foo'" "" { target *-*-* } .-2 }
// { dg-message "because conversion sequence for the argument is better" "" { target *-*-* } .-3 }
}
