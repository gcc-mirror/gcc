// PR c++/98077
// { dg-do compile { target c++17 } }
// A variant of class-deduction102.C where the template placeholder is a template
// template parameter.

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

template<class F, template<class> class Tmpl>
using CallableTraitT = CallableTrait<decltype(Tmpl{F()})>;

template<class F, template<class> class Tmpl>
using ReturnType = typename CallableTraitT<F, Tmpl>::ReturnType;

using type = ReturnType<int(*)(), function>;
using type = int;
