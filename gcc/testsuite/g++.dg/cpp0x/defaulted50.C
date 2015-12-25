// PR c++/69005
// { dg-do compile { target c++11 } }

template<typename T> T& declval();

template<typename _Sig> class function;

template<typename _Res, typename _Arg>
struct function<_Res(_Arg)>
{
  function() noexcept { }

  function(const function&) { }

  template<typename _Functor,
	   typename = decltype(declval<_Functor&>()(declval<_Arg>()))>
  function(_Functor) { }

  _Res operator()(_Arg) const;
};

struct Foo {
  function<void(Foo)> Func;
};

extern Foo exfoo;
Foo f (exfoo);
