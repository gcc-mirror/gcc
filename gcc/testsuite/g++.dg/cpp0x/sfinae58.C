// PR c++/71568
// { dg-do compile { target c++11 } }

template <typename T> class F : T {};
template <typename> using void_t = void;
template <class, class = void> struct G;
template <typename T> struct G<T, void_t<decltype(&T::nlog_custom)>> {};
struct D {
  void nlog_custom();
};
G<F<D>> g;			// { dg-error "incomplete" }

