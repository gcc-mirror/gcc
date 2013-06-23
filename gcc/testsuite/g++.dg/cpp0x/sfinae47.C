// Source: Comment 16 of PR51213
// { dg-do compile { target c++11 } }

template <class T>
T && declval();

template <class T>
constexpr auto hasSize(int) -> decltype(declval<T&>().size(), bool())
{ return true; }

template <class T>
constexpr bool hasSize(...)
{ return false; }

struct A
{
  int size();
};

struct B : private A
{
};

static_assert(hasSize<A>(0),  "A");
static_assert(!hasSize<B>(0), "B");
