// { dg-do link { target c++11 } }
// { dg-require-effective-target lto }
// { dg-options "-O2 -flto -fdump-rtl-loop2_unroll" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib rand" { ! hostedlib } }

#include <cstdlib>

template<typename Iter, typename Pred>
inline Iter
my_find(Iter first, Iter last, Pred pred)
{
#pragma GCC unroll 4
#pragma GCC novector
    while (first != last && !pred(*first))
        ++first;
    return first;
}

__attribute__((noipa))
short *use_find(short *p)
{
    auto pred = [](short x) { return x == 42; };
    return my_find(p, p + 1024, pred);
}

int main(void)
{
  short a[1024];
#pragma GCC unroll 0
  for (int i = 0; i < 1024; i++)
    a[i] = rand ();

  return use_find (a) - a;
}

// { dg-final { scan-ltrans-rtl-dump-times "Unrolled loop 3 times" 1 "loop2_unroll" } }
