// DR 3045 - Regularizing environment interactions of expansion statement
// { dg-do compile { target c++11 } }
// { dg-options "" }

void
foo ()
{
  static constexpr int arr[] = { 1, 2, 3 };
  for (auto g : arr) {				// { dg-message "'int g' previously declared here" }
    int g = 42;					// { dg-error "redeclaration of 'int g'" }
  }
  template for (auto x : arr) {			// { dg-warning "'template for' only available with" "" { target c++23_down } }
						// { dg-message "'int x' previously declared here" "" { target *-*-* } .-1 }
    int x = 42;					// { dg-error "redeclaration of 'int x'" }
  }
}
