// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
  concept bool One() { return sizeof(T) >= 4; }

template<typename T>
  concept bool Two() { return One<T>() && sizeof(T) >= 8; }

// Check ordering of partial specializaitons
template<typename T>
  struct S2 { static const int value = 0;  };

template<One T>
  struct S2<T> { static const int value = 1; };

template<Two T>
  struct S2<T> { static const int value = 2; };

struct one_type { char x[4]; };
struct two_type { char x[8]; };

static_assert(S2<char>::value == 0, "");
static_assert(S2<one_type>::value == 1, "");
static_assert(S2<two_type>::value == 2, "");

int main() { }
