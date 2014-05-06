// PR c++/60999
// { dg-do compile { target c++11 } }

template <typename A>
struct foo
{
};
  
template<>
struct foo<int>
{
  static constexpr int code = 42;
  unsigned int bar = static_cast<unsigned int>(code);
};
  
foo<int> a;
