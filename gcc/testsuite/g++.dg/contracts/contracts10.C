// general checks to ensure that contract violations are generated during
// runtime when appropriate
// each check also validates the expected file name, line number, function,
// predicate, and contract level are included in the violation_info object
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-build-level=audit -fcontract-continuation-mode=on" }

namespace tns
{
  int fun()
  {
    int x = 1;
    [[ assert: x < 0 ]];
    return 0;
  }
  int fun2();

  struct TestType
  {
    static int fun();
    static int fun2()
    {
      int x = 1;
      [[ assert: x < 0 ]];
      return 0;
    }
  };
}

int tns::fun2()
{
  int x = 1;
  [[ assert: x < 0 ]];
  return 0;
}

int tns::TestType::fun()
{
  int x = 1;
  [[ assert: x < 0 ]];
  return 0;
}

int main()
{
  int x = 100;
  [[assert: x < 0]];
  [[assert default: x < 1]];
  [[assert audit: x < 2]];
// contract_violation.line_number() may eventually come from
// std::source_location which *is* affected by the #line macro; our current
// implementation conforms to this so we've included it as a check
#line 100
  [[assert: x < 3]];
  [[assert axiom: x < 4]];

  tns::fun();
  tns::fun2();

  tns::TestType::fun();
  tns::TestType::fun2();
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 47 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 48 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 49 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 100 main .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 13 tns::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 33 tns::fun2 .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 40 tns::TestType::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 24 tns::TestType::fun2 .*(\n|\r\n|\r)*" }
 
