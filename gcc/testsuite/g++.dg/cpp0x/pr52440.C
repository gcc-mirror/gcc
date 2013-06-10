// PR c++/52440
// { dg-do compile { target c++11 } }

template<bool>
struct V
{
  typedef void type;
};

template<typename T>
struct X
{
  template<typename>
  static constexpr bool always_true()
  {
    return true;
  }

  template<typename U,
           typename = typename V<always_true<U>()>::type>
  X(U &&) {}
};

int main()
{
  X<int> x(42);
}
