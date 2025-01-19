// P2662R3 - Pack Indexing
// PR c++/113798
// { dg-do run { target c++26 } }

#include <initializer_list>

template<typename... Ts>
int
g (auto... Is)
{
  std::initializer_list<Ts...[0]> l{ Is...[0], Is...[1], Is...[2], Is...[3], Is...[4] };
  int sum = 0;
  for (auto x : l)
    sum += x;
  return sum;
}

int
main ()
{
  if (g<int> (1, 2, 3, 4, 5) != 15)
    __builtin_abort ();
}
