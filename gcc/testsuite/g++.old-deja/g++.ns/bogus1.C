// { dg-do assemble  }

namespace N {}

void f(int N::k); // { dg-error "" } 

class Foo
{
  int N::j; // { dg-error "" } invalid use of `::'
};
