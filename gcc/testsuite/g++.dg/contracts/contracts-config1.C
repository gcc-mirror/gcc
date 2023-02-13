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

// { dg-output {contract violation in function main at .*:14: false(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:15: false(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:16: false(\n|\r\n|\r)} }
// { dg-output {\[level:audit, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:17: false(\n|\r\n|\r)} }
// { dg-output {\[role:new_role, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:18: false(\n|\r\n|\r)} }
// { dg-output {\[role:new_role, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:19: false(\n|\r\n|\r)} }
// { dg-output {\[level:audit, role:new_role, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:20: false(\n|\r\n|\r)} }
// { dg-output {\[level:, role:, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:21: false(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:22: false(\n|\r\n|\r)} }
// { dg-output {\[level:audit, continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function fun at .*:7: r > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
