// generic assert contract parsing checks
//   check omitted, 'default', 'audit', and 'axiom' contract levels parse
//   check that all concrete semantics parse
//   check omitted, '%default' contract roles parse
//   ensure that an invalid contract level 'invalid' errors
//   ensure that a predicate referencing an undefined variable errors
//   ensure that a missing colon after contract level errors
//   ensure that an invalid contract role 'invalid' errors
//   ensure that a missing colon after contract role errors
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

static_assert (__cpp_contracts >= 201906);
static_assert (__cpp_contracts_literal_semantics >= 201906);
static_assert (__cpp_contracts_roles >= 201906);

int main()
{
  int x;

  [[assert: x >= 0]];
  [[assert default: x < 0]];
  [[assert audit: x == 0]];
  [[assert axiom: x == 1]];

  [[assert: x > 0 ? true : false]];
  [[assert: x < 0 ? true : false]];

  [[assert: x = 0]]; // { dg-error "expected .]. before .=. token" }

  [[assert ignore: x >= 0]];
  [[assert assume: x >= 0]];
  [[assert check_never_continue: x >= 0]];
  [[assert check_maybe_continue: x >= 0]];

  [[assert %default: x >= 0]];
  [[assert default %default: x < 0]];
  [[assert audit %default: x == 0]];
  [[assert axiom %default: x == 1]];

  [[assert check_always_continue: x >= 0]]; // { dg-error "expected contract level" }
  [[assert invalid: x == 0]]; // { dg-error "expected contract level" }
  [[assert: y == 0]]; // { dg-error ".y. was not declared in this scope" }
  [[assert default x == 0]]; // { dg-error "expected .:. before .x." }
  [[assert %default x >= 0]]; // { dg-error "expected .:. before .x." }

  [[assert %invalid: x >= 0]]; // TODO: optional warning?
  return 0;
}
