// PR c++/109095
// { dg-do compile { target c++20 } }

template< typename T >
struct bar
{};

template< int X >
struct baz
{};

template< auto N, template< auto N2 > typename TT >
struct foo;

template< typename T, bar< T > B, template< T N2 > typename TT >
struct foo< B, TT >
{};

foo< bar< int >{}, baz > x;
