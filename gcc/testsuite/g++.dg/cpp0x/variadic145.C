// PR c++/59730
// { dg-do compile { target c++11 } }

template <typename> void declval();
template <typename> void forward();
template <typename> class D;
template <typename _Functor, typename... _Bound_args>
class D <_Functor(_Bound_args...)> {
  template <typename... _Args, decltype(declval<_Functor>)>
  void operator()(...) {
    0(forward<_Args>...);
  }
};
