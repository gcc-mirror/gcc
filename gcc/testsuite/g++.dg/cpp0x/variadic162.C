// PR c++/63405
// { dg-do compile { target c++11 } }

template <typename _Tp> _Tp forward(_Tp);
template <typename Args> struct Format { Format(int, Args); };
template <typename... Args> auto format(Args &&... args) -> Format<Args...> {
  return {0, args...};
}

template <typename... Args> void msg(Args... args) {
  format(forward(args)...);
}

void some_function() { msg('x'); }
