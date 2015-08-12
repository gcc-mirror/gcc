// PR c++/66405
// { dg-do compile { target c++11 } }

template <typename T, T...> struct B;
template <bool... Bools> using and_c = B<bool, +Bools...>;
template <typename T, typename U> using Constructible = int;
template <typename... Ts> struct common_tuple {
  template <typename... Us,
	    typename = and_c<Constructible<Ts, Us>{}...> >
    common_tuple();
  void foo();
};
template <> void common_tuple<>::foo(){}
