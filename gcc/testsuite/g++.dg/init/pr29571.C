// PR c++/29571

struct A
{
  static const int i = 0/0 + ""; // { dg-warning "division by zero" }
  // { dg-error "constant|conversion|initializ" "" { target *-*-* } 5 }
  static const int j = int(i);
};

// Currently G++ complains about a non-constant initializer for 'j' in
// C++11 mode, but not C++98.  Either way is correct because it depends on
// the erroneous initializer for i, so don't require the error.
// { dg-prune-output ":7:" }
