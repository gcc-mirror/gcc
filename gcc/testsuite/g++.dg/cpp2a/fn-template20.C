// PR c++/93807
// { dg-do compile { target c++11 } }

// In C++17, we need the following declaration to treat operator== as
// a template name.  In C++20, this is handled by [temp.names]/2.
#if __cplusplus <= 201703L
template <typename T>
class Foo;
template <typename T>
constexpr bool operator==(T lhs, const Foo<T>& rhs);
#endif

template <typename T>
class Foo {
public:
  constexpr Foo(T k) : mK(k) {}

  constexpr friend bool operator==<T>(T lhs, const Foo& rhs);
private:
  T mK;
};

template <typename T>
constexpr bool
operator==(T lhs, const Foo<T>& rhs)
{
  return lhs == rhs.mK;
}

int
main ()
{
  return 1 == Foo<int>(1) ? 0 : 1;
}
