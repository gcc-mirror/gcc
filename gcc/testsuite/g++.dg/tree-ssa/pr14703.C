/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-ipa-reference" } */
#include <iostream>

namespace {
template <unsigned long long L> class fib {
 public:
   static const unsigned long long value = fib<L - 1>::value + fib<L - 2>::value;
};

template <> class fib<0> {
 public:
   static const unsigned long long value = 1;
};

template <> class fib<1> {
 public:
   static const unsigned long long value = 1;
};

template<unsigned long long L> inline unsigned long long fibconst()
{
   return fibconst<L - 1>() + fibconst<L - 2>();
}

template <> inline unsigned long long fibconst<0>()
{
   return 1ull;
}

template <> inline unsigned long long fibconst<1>()
{
   return 1ull;
}

template <> inline unsigned long long fibconst<2>()
{
   return 2ull;
}

}

int main()
{
   ::std::cerr << "fib<90>::value == " << fib<90>::value << "\n";
   ::std::cerr << "fibcst<90>() == " << fibconst<90>() << "\n";
}
// { dg-final { scan-tree-dump-not "fibconst" "optimized" } }
// { dg-final { cleanup-tree-dump "optimized" } }
