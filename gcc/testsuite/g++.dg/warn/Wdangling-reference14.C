// PR c++/113256
// { dg-do compile { target c++14 } }
// { dg-options "-Wdangling-reference" }

#include <utility>
#include <cassert>

template<class M, class T, class A> auto bind(M T::* pm, A)
{
    return [=]( auto&& x ) -> M const& { return x.*pm; };
}

template<int I> struct arg {};

arg<1> _1;

int main()
{
    std::pair<int, int> pair;
    int const& x = bind( &std::pair<int, int>::first, _1 )( pair ); // { dg-bogus "dangling reference" }
    assert( &x == &pair.first );
}
