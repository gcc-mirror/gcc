// check that a valid program using assertions compiles and runs
//   ensure an axiom with a failing predicate doesn't prevent a successful run
//   (axiom level contracts are never checked at runtime)
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-role=custom:never,ignore,ignore" }

int main()
{
  int x = 1;
  [[assert axiom: x < 0]];
  [[assert %custom: x > 0]];
  [[assert audit %custom: x < 0]];
  return 0;
}

