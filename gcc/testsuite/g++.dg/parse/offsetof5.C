// PR c++/16618

#include <stddef.h>

struct test
{
  const char a;
};

int main()
{
  offsetof(test,a);
}
