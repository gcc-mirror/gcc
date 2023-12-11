// A version of concept-6.h using an alias template + alias CTAD

template<typename _Callable>
struct Base
{
  Base (const _Callable &)
    requires true
  {}
};

template<typename _Callable> requires true
using Derived = Base<_Callable>;

inline Derived all = [] (auto&& __r) {};
