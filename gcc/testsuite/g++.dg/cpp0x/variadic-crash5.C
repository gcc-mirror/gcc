// PR c++/81957
// { dg-do compile { target c++11 } }

template <class T, T v>
struct integral_constant { };

struct f {
  template<bool b, typename Int>
  void operator()(integral_constant<bool,b>, Int i) {
  }
};

template<bool...Bs, typename F, typename ...T>
auto dispatch(F f, T...t) -> decltype(f(integral_constant<bool,Bs>()..., t...)) {
  return f(integral_constant<bool,Bs>()..., t...);
}

template<bool...Bs, typename F, typename ...T>
auto dispatch(F f, bool b, T...t) -> decltype(dispatch<Bs..., true>(f, t...)) {
  if (b)
    return dispatch<Bs..., true>(f, t...);
  else
    return dispatch<Bs..., false>(f, t...);
}

int main() {
  dispatch(f(), true, 5);
}
