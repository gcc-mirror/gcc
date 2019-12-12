// { dg-do compile { target c++2a } }
// { dg-additional-options "-fconcepts-ts" }

template<class T>
concept bool C = requires(const T& t) { t.foo(); };

template<class T>
struct Base
{
  constexpr T const& derived() const
  {
    return static_cast<T const&>(*this);
  }
  constexpr bool bar() const
    requires requires(const T& t) { t.foo(); }
  {
    derived().foo();
    return true;
  }
};

template<class T>
struct Derived : Base<Derived<T>>
{
  constexpr void foo() const {}
};

int main()
{
  static_assert(Derived<int>{}.bar());
}

