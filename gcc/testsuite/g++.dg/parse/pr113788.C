// PR c++/113788
// { dg-do compile { target c++11 } }

struct S { int a, b; };
struct U {
  void foo () { this int g = 1; }	// { dg-error "expected ';' before 'int'" }
};
this auto h = 1;			// { dg-error "expected unqualified-id before 'this'" }

int
main ()
{
  S s = { 1, 2 };
  short t[3] = { 3, 4, 5 };
  this auto &[a, b] = s;		// { dg-error "invalid use of 'this' in non-member function" }
  this auto &[c, d, e] = t;		// { dg-error "invalid use of 'this' in non-member function" }
  this int f = 1;			// { dg-error "invalid use of 'this' in non-member function" }
  for (this auto &i : t)		// { dg-error "invalid use of 'this' in non-member function" }
    ;					// { dg-error "expected" }
}					// { dg-error "expected" }
