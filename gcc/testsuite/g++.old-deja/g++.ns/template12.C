// Build don't link:
// Templates can be defined outside of the namespace if the have been declared
// inside
namespace bar
{
  template <typename T>
  T const foo(T const &);
  template<> const int foo<int>(int const &);
}

template <typename T>
T const
bar::foo(T const &a)
{
  return a;
}

template<> const int bar::foo<int>(int const &){return 0;}

