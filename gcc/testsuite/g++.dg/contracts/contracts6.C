// ensure a default level assert with a failing predicate does not generate an
// error during runtime when the contract build level is off
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-build-level=off" }

int main()
{
  int x = 1;
  [[assert default: x < 0]];
  return 0;
}
