// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool Class() { return __is_class(T); }

template<typename T>
  concept bool Union() { return __is_union(T); }


// Check ordering of specializations
template<typename T>
  concept bool One() { return sizeof(T) >= 4; }

template<typename T>
  concept bool Two() { return One<T>() && sizeof(T) >= 8; }

// Check non-overlapping specializations
template<typename T>
  struct S1 { static const int value = 0; };

template<Class T>
  struct S1<T> { static const int value = 1; };

template<Union T>
  struct S1<T> { static const int value = 2; };

struct S { };
union U { };

static_assert(S1<int>::value == 0, "");
static_assert(S1<S>::value == 1, "");
static_assert(S1<U>::value == 2, "");


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
