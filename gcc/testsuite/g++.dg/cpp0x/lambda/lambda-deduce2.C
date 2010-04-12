// Test that cv-quals are dropped from non-class return type
// { dg-options "-std=c++0x" }

template <class T, class U>
struct assert_same_type;
template <class T>
struct assert_same_type<T,T> { };

struct A
{
  int i;
};

extern const int i;
assert_same_type <decltype ([]{ return i; }()), int> x;
