// PR c++/56970
// { dg-do compile { target c++11 } }

template <typename T>
struct has
{
  template <typename>
  constexpr static int test(...) {
    return 0;
  }
  
  template <typename C>
  constexpr static int test(decltype(sizeof(C::x))) {  // Doesn't compile.
    return 1;   // Is a member variable.
  }
  
  template <typename C, int c = sizeof(decltype(((C*)nullptr)->x()))>
  constexpr static int test(int) {
    return 2;   // Is a member function.
  }

  static const int value = test<T>(0);
};

struct foo {
  int x;
};

struct bar {
  int x();
};

static_assert(has<int>::value == 0, "");
static_assert(has<foo>::value == 1, "");
static_assert(has<bar>::value == 2, "");
