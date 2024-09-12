// ensure an audit level assert with a failing predicate generates an error
// during runtime when the contract build level is audit
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-build-level=audit" }
// { dg-shouldfail "assert violation" }
// { dg-output "contract violation in function main" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

int main()
{
  int x = 1;
  [[assert audit: x < 0]];
  return 0;
}
