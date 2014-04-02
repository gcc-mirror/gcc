// PR c++/52363
// { dg-do compile { target c++11 } }

#include <type_traits>

struct proxy
{
  void operator=(int const&);
  void operator=(int&&) const;
};

static_assert( !std::is_assignable<proxy, int>::value, "" );
static_assert( std::is_assignable<const proxy, int>::value, "" );
