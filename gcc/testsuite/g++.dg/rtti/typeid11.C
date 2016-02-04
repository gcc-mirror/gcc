// { dg-do run }

#include <typeinfo>

struct Base { virtual void foo() {} }; // polymorphic

int main()
{
  Base b;
  Base *ary[] = { &b, &b, &b};

  int iter = 0;
  typeid(*ary[iter++]);
  if (iter != 1)	// should be 1
    __builtin_abort();	// but 2
}
