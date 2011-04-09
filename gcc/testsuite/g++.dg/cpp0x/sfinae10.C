// PR c++/48452
// { dg-options -std=c++0x }
namespace std {
  template <class T> T&& declval();
}

template<class T, class... Args>
decltype(T(std::declval<Args>()...), char()) f(int);

template<class, class...>
char (&f(...))[2];

struct B {};

static_assert(sizeof(f<B, void, int>(0)) != 1, "Error"); // b
static_assert(sizeof(f<void, int, int>(0)) != 1, "Error"); // c
