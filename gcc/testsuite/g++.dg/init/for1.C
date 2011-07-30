// PR c++/13865
// Bug: We were destroying 'a' before executing the loop.

// { dg-do run }

#include <stdio.h>

int i;
int r;

class A
{
public:
  A() { printf("A ctor\n"); }
  ~A()
  {
    printf("A dtor\n");
    if (i != 2)
      r = 1;
  }
};

int main(int argc, char **argv)
{
  for (A a; i < 2; ++i) {
    printf("iteration %d\n", i);
  }

  return r;
}
