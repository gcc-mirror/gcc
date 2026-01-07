// PR c++/119580
// { dg-do compile { target c++11 } }

template<typename> struct V;
template<typename> class C;
class F;

struct S {
  template<typename>
  static void foo ();
  template<typename T, typename>
  C<decltype(S::foo<T::value_type>)> foo ();
  decltype(foo<V<F>>()) *a;
};

S s;
