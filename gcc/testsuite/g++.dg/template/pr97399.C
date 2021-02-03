// PR c++/97399
// { dg-do compile { target c++11 } }

template <bool> struct enable_if_t {};

struct tmp {
  template <class> static constexpr bool is_integral();
  template <class T> static auto f()
    -> enable_if_t<tmp::is_integral<T>()>;
  template <class T> friend auto g(tmp, T)
    -> enable_if_t<!tmp::is_integral<T>()>;
};

template <class> constexpr bool tmp::is_integral() { return true; }

template <class T> auto tmp::f()
  -> enable_if_t<tmp::is_integral<T>()> { return {}; }

int main()
{
  tmp::f<int>();
  g(tmp{}, 0);
}
