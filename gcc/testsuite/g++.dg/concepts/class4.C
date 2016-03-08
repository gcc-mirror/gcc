// { dg-options "-std=c++1z -fconcepts" }

template<typename T>
  concept bool Class() { return __is_class(T); }

template<typename T>
  concept bool Union() { return __is_union(T); }

// Check non-overlapping specializations
template<typename T> struct S1 { static const int value = 0; };
template<Class T> struct S1<T> { static const int value = 1; };
template<Union T> struct S1<T> { static const int value = 2; };

struct S { };
union U { };

static_assert(S1<int>::value == 0, "");
static_assert(S1<S>::value == 1, "");
static_assert(S1<U>::value == 2, "");

int main() { }
