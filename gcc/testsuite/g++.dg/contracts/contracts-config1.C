// Small test to ensure that the level and role information printed by various
// contract configurations is correct.
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-role=default:maybe,maybe,ignore" }

int fun(int n)
  [[ post default r: r > 0 ]]
{
  return -n;
}

int main(int, char **)
{
  [[ assert default: false ]];
  [[ assert: false ]];
  [[ assert audit: false ]];
  [[ assert default %new_role: false ]];
  [[ assert %new_role: false ]];
  [[ assert audit %new_role: false ]];
  [[ assert check_maybe_continue: false ]];
  [[ assert %default: false ]];
  [[ assert audit %default: false ]];
  fun(5);
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*main false default default 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false default default 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false audit default 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false default new_role 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false default new_role 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false audit new_role 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false   1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false default default 1.*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*main false audit default 1.*(\n|\r\n|\r)*" }

