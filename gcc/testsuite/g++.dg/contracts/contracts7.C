// ensure a default level assert with a failing predicate does generates an
// error during runtime but lets the program continue and complete
// successfully when the contract build level is default but continuation on
// contract failure is switched on
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
// { dg-output "contract violation in function main" }

int main()
{
  int x = 1;
  [[assert default: x < 0]];
  return 0;
}
