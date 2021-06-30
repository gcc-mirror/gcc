// check that a valid program using assertions compiles and runs
//   ensure an axiom with a failing predicate doesn't prevent a successful run
//   (axiom level contracts are never checked at runtime)
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-role=custom:maybe,maybe,ignore" }

int main()
{
  int x = 10;
  [[assert axiom: x < 0]];
  [[assert %custom: x < 0]];
  [[assert audit %custom: x < 1]];
  [[assert axiom %custom: x < 1]];
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 11 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 main .*(\n|\r\n|\r)*" }

