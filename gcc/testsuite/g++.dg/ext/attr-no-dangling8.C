// PR c++/110358
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }

template<class T> constexpr bool is_reference_v = false;
template<class T> constexpr bool is_reference_v<T&> = true;
template<class T> constexpr bool is_reference_v<T&&> = true;

template <typename T>
struct [[gnu::no_dangling(is_reference_v<T>)]] S {
  int i;
  int &foo (const int &);
};

template <typename T1, typename T2>
struct X {
  template <typename U1 = T1, typename U2 = T2>
  struct [[gnu::no_dangling(is_reference_v<U1> && is_reference_v<U2>)]] Y {
    int i;
    int &foo (const int &);
  };
};

void
g ()
{
  [[maybe_unused]] const int &x0 = S<int&>().foo (42);  // { dg-bogus "dangling" }
  [[maybe_unused]] const int &x1 = S<int>().foo (42);   // { dg-warning "dangling" }
  [[maybe_unused]] const auto &x2 = X<int, int&>::Y<>().foo (42); // { dg-warning "dangling" }
  [[maybe_unused]] const auto &x3 = X<int&, int&>::Y<>().foo (42); // { dg-bogus "dangling" }
}

