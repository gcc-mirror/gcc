// { dg-do run }

#include <cstdlib>

enum foo
{
  foo1   = 0,
  foo2   = 0xffffffffffffffffULL,
  foo3   = 0xf0fffffffffffffeULL
};

int main ()
{
  if (sizeof (enum foo) != sizeof (unsigned long long))
    std::abort ();
}
