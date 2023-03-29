// ensure that dependent and non-dependent asserts inside templated
// functions parse without error whether the function is instatiated or not
// ensure that assert contract checks are generated inside called templates
// ensure that template functions can be used as assert predicates
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

template<typename T>
int fun1(int a, T b)
{
  [[ assert: a > 0 ]];
  [[ assert: (long long)b > 0 ]];
  return a > 0;
}

template<typename T>
struct test
{
  static int fun(int a, T b) {
    [[ assert: a > 0 ]];
    [[ assert: b > 0 ]];
    return a > 0;
  }
};

int main()
{
  fun1(1, -1);
  fun1(-1, 1.0);
  fun1(-1, "test");

  [[ assert: fun1(-1, -5) ]];
  [[ assert: test<int>::fun(10, -6) ]];
  [[ assert: test<double>::fun(10.0, -7) ]];
  // return 0;
}

// { dg-output {contract violation in function fun1<int> at .*:12: \(long long\)b > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function fun1<double> at .*:11: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function fun1<const char\*> at .*:11: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function fun1<int> at .*:11: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function fun1<int> at .*:12: \(long long\)b > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function main at .*:32: fun1\(-1, -5\)(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function test<int>::fun at .*:21: b > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function test<double>::fun at .*:21: b > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
