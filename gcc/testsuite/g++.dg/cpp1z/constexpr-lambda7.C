// Testcase from P0170R1
// { dg-options -std=c++17 }

auto ID = [](auto a) { return a; };
static_assert( ID (3) == 3); // OK
struct NonLiteral {
  NonLiteral(int n) : n(n) { }
  int n;
};

static_assert( ID (NonLiteral{3}).n == 3); // { dg-error "non-literal" }
// { dg-prune-output "static assertion" }
