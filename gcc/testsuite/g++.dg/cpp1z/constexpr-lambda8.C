// Testcase from P0170R1
// { dg-do compile { target c++17 } }

auto Fwd = [](int (*fp)(int), auto a) { return fp(a); };
auto C = [](auto a) { return a; };
static_assert( Fwd(C ,3) == 3); // OK

// No specialization of the function call operator template can be constexpr
// (because of the local static).
auto NC = [](auto a) { static int s; return a; }; // { dg-error "static" }
// { dg-error "called in a constant expression" "" { target *-*-* } .-1 }

static_assert( Fwd(NC ,3) == 3); // { dg-error "" }
// { dg-message "operator int" "" { target *-*-* } .-1 }

// We look for the string "operator int" to check that we aren't trying to do
// template pretty-printing in an expression; that gets incredibly unwieldy
// with the decltype magic we do for lambdas.
