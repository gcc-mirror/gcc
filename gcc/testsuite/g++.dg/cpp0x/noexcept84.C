// PR c++/114114
// { dg-do compile { target c++11 } }

template<bool B>
constexpr void
test ()
{
  constexpr bool is_yes = B;
  struct S {
    constexpr S() noexcept(is_yes) { }
  };
  S s;
}

constexpr bool foo() { return true; }

template<typename T>
constexpr void
test2 ()
{
  constexpr T (*pfn)() = &foo;
  struct S {
    constexpr S() noexcept(pfn()) { }
  };
  S s;
}

int main()
{
  test<true>();
  test2<bool>();
}
