// basic test to ensure contracts work pre-c++2a
// { dg-do run { target c++11 } }
// { dg-additional-options "-fcontracts -fcontract-continuation-mode=on" }

int f(int n)
  [[ pre: n > 0 ]]
  [[ post r: r < 0 ]]
{
  [[ assert: n > 0 ]];
  return -n;
}

int main()
{
  f(-5);
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 6 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 9 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 7 .*(\n|\r\n|\r)*" }

