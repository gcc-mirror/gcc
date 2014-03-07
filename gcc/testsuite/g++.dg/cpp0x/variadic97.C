// PR c++/42266
// { dg-do compile { target c++11 } }

template<typename... _Elements>
  class tuple;

template<typename _Arg>
  class _Mu;

template<typename _Signature>
  struct _Bind;

template<typename _Functor, typename... _Bound_args>
  class _Bind<_Functor(_Bound_args...)>
  {
    template<typename... _Args, typename
             = decltype(_Functor()(_Mu<_Bound_args>()(_Bound_args(),
                                                      tuple<_Args...>())...) )>
      void __call() { }
  };

template<typename _Functor, typename _Arg>
  _Bind<_Functor(_Arg)>
  bind(_Functor, _Arg) { }

struct State
{
  bool ready() { return true; }

  void f()
  {
    bind(&State::ready, this);
  }
};

