// simple check to ensure we don't emit a function with the same name twice,
// when wrapping functions in pre- and postconditions.
// { dg-do link }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

volatile int x = 10;

extern "C" void
f ()
  [[ pre: x < 10 ]]
{
}

int
main ()
  [[ post: x > 10 ]]
{
  f();
}
