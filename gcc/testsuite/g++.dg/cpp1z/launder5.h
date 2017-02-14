#ifndef GCC_TEST_LAUNDER5_H
#define GCC_TEST_LAUNDER5_H

struct A
{
  const int x;
};

struct B
{
  A a;
};

void f(B& b);

#endif
