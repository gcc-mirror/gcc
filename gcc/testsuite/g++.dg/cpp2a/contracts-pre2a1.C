// ensure the feature test macros are defined pre c++20 while we still support
// -fcontracts independent of std version
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fcontracts" }

static_assert (__cpp_contracts >= 201906, "__cpp_contracts");
static_assert (__cpp_contracts_literal_semantics >= 201906, "__cpp_contracts_literal_semantics");
static_assert (__cpp_contracts_roles >= 201906, "__cpp_contracts_roles");

int main()
{
  int x;

  [[assert: x >= 0]];
  [[assert default: x < 0]];
  [[assert audit: x == 0]];
  [[assert axiom: x == 1]];

  [[assert: x > 0 ? true : false]];
  [[assert: x < 0 ? true : false]];

  [[assert ignore: x >= 0]];
  [[assert assume: x >= 0]];
  [[assert check_never_continue: x >= 0]];
  [[assert check_maybe_continue: x >= 0]];

  [[assert %default: x >= 0]];
  [[assert default %default: x < 0]];
  [[assert audit %default: x == 0]];
  [[assert axiom %default: x == 1]];
  return 0;
}

