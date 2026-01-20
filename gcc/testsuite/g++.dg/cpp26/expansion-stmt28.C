// PR c++/123694
// { dg-do compile { target c++11 } }
// { dg-options "-Wmisleading-indentation" }

int
foo ()
{
  int b = 0;
  template for (constexpr auto a : { 1, 2L, 3.0 })	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    b += a;						// { dg-warning "this 'template for' clause does not guard\\.\\.\\." "" { target *-*-* } .-1 }
    b++;						// { dg-message "\\.\\.\\.this statement, but the latter is misleadingly indented as if it were guarded by the 'template for'" }
  return b;
}
