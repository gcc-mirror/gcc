// PR c++/87075
// { dg-do compile { target c++14 } }

template <typename T>
struct vec
{
  struct { T y; } n;
  vec() = default;
};

template <typename T>
struct S
{
  vec<T> value[2];
  template<typename U>
  constexpr S(const U&);
};

template<typename T>
template<typename X>
constexpr S<T>::S(const X&)
{
  value[0] = vec<T>();
}

S<float>m(0);
