#ifndef GCC_TEST_LAUNDER6_H
#define GCC_TEST_LAUNDER6_H

struct A
{
  int& x;
};

struct B
{
  A a;
};

void f(B& b);

#endif
