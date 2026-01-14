// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// US 112-172

#include <meta>
using namespace std::meta;

struct B { };
struct D : B { };

template<typename T>
consteval bool
can_extract (info r)
{
  try { extract<T>(r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

constexpr B arrb[] = { {}, {}, {} };
constexpr D arrd[] = { {}, {}, {} };
static_assert (can_extract<const D *>(^^arrd));
static_assert (can_extract<const B *>(^^arrb));
static_assert (!can_extract<const B *>(^^arrd));
static_assert (!can_extract<const D *>(^^arrb));
