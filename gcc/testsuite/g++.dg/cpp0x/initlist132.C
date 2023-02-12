// PR c++/108071
// { dg-do compile { target c++11 } }

#include <initializer_list>

template< typename T1, typename T2 = void >
struct ConstCharArrayDetector
{
    static const bool ok = false;
};
template< std::size_t N, typename T >
struct ConstCharArrayDetector< const char[ N ], T >
{
    typedef T Type;
};

struct Dummy { };

struct OUString
{
  template<typename T>
    OUString(T&, typename ConstCharArrayDetector<T, Dummy>::Type = Dummy())
    { }
};

struct Sequence {
  Sequence(std::initializer_list<OUString>);
};

Sequence s = {""};
