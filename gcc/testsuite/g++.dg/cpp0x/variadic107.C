// PR c++/48451
// { dg-options -std=c++11 }

namespace std {
  template <class T> T&& declval();
}

template<class T, class... Args,
 class = decltype(T(std::declval<Args>()...))
 >
char f(int);

struct From2Ints { From2Ints(int, int); };

static_assert(sizeof(f<From2Ints, int, int>(0)) == 1, "Error"); // b
