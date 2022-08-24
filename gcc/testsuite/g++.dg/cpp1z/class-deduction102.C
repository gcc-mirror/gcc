// PR c++/98077
// { dg-do compile { target c++17 } }

template<class R>
struct function {
  template<class T> function(T);
  using type = R;
};

template<class T> function(T) -> function<decltype(T()())>;

template<class T>
struct CallableTrait;

template<class R>
struct CallableTrait<function<R>> { using ReturnType = R; };

template<class F>
using CallableTraitT = CallableTrait<decltype(function{F()})>;

template<class F>
using ReturnType = typename CallableTraitT<F>::ReturnType;

using type = ReturnType<int(*)()>;
using type = int;
