// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct Foo
{
  int x;
  bool y;
  long z[4];
};

Foo foo() [[ pre: true ]]
{
  return {};
}

