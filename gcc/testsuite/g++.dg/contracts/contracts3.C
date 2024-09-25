// ensure a default level assert with a failing predicate generates an error
// during runtime when the contract build level is default
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
// { dg-shouldfail "assert violation" }
// { dg-output "contract violation in function main" }
// { dg-skip-if "requires hosted libstdc++ for stdc++exp" { ! hostedlib } }

int main()
{
  int x = 1;
  [[assert default: x < 0]];
  return 0;
}
