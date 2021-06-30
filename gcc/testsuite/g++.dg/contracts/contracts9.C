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

// { dg-output "default std::handle_contract_violation called: .*.C 12 fun1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 11 fun1<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 11 fun1<const char.> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 11 fun1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 fun1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 32 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 21 test<int>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 21 test<double>::fun .*(\n|\r\n|\r)*" }
