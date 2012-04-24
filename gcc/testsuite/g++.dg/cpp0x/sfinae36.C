// PR c++/52363
// { dg-options "-std=c++11 -pedantic" }

#include <type_traits>

struct proxy
{
  void operator=(int const&);
  void operator=(int&&) const;
};

static_assert( !std::is_assignable<proxy, int>::value, "" );
static_assert( std::is_assignable<const proxy, int>::value, "" );
