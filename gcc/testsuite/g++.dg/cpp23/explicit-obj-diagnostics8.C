// P0847R7
// { dg-do compile { target c++23 } }

// xobj lambda with invalid decl specs

void test()
{
  auto f0 = [](this auto) mutable {}; // { dg-line line_f0 }
  auto f1 = [](this auto&) mutable {}; // { dg-line line_f1 }
  auto f2 = [](this auto const&) mutable {}; // { dg-line line_f2 }
  auto f3 = [](this auto&&) mutable {}; // { dg-line line_f3 }

  auto g0 = [](this auto) static {}; // { dg-line line_g0 }
  auto g1 = [](this auto&) static {}; // { dg-line line_g1 }
  auto g2 = [](this auto const&) static {}; // { dg-line line_g2 }
  auto g3 = [](this auto&&) static {}; // { dg-line line_g3 }

  auto fc0 = [n = 0](this auto) mutable {}; // { dg-line line_fc0 }
  auto fc1 = [n = 0](this auto&) mutable {}; // { dg-line line_fc1 }
  auto fc2 = [n = 0](this auto const&) mutable {}; // { dg-line line_fc2 }
  auto fc3 = [n = 0](this auto&&) mutable {}; // { dg-line line_fc3 }

  auto gc0 = [n = 0](this auto) static {}; // { dg-line line_gc0 }
  auto gc1 = [n = 0](this auto&) static {}; // { dg-line line_gc1 }
  auto gc2 = [n = 0](this auto const&) static {}; // { dg-line line_gc2 }
  auto gc3 = [n = 0](this auto&&) static {}; // { dg-line line_gc3 }
}

// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_f0 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_f1 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_f2 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_f3 }

// { dg-note {the passed in closure object will not be mutated because it is taken by value} {} { target *-*-* } line_f0 }
// { dg-note {explicit object parameter is already a mutable reference} {} { target *-*-* } line_f1 }
// { dg-note {declare the explicit object parameter as non-const reference instead} {} { target *-*-* } line_f2 }
// { dg-note {explicit object parameter is already a mutable reference} {} { target *-*-* } line_f3 }

// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_g0 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_g1 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_g2 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_g3 }

// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_g0 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_g1 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_g2 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_g3 }

// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_fc0 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_fc1 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_fc2 }
// { dg-error {'mutable' lambda specifier with explicit object parameter} {} { target *-*-* } line_fc3 }

// { dg-note {the passed in closure object will not be mutated because it is taken by value} {} { target *-*-* } line_fc0 }
// { dg-note {explicit object parameter is already a mutable reference} {} { target *-*-* } line_fc1 }
// { dg-note {declare the explicit object parameter as non-const reference instead} {} { target *-*-* } line_fc2 }
// { dg-note {explicit object parameter is already a mutable reference} {} { target *-*-* } line_fc3 }

// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_gc0 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_gc1 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_gc2 }
// { dg-error {'static' lambda specifier with explicit object parameter} {} { target *-*-* } line_gc3 }

// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_gc0 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_gc1 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_gc2 }
// { dg-note {explicit object parameter declared here} {} { target *-*-* } line_gc3 }

