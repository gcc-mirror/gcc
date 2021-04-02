
template<typename _Callable>
struct Base
{
  Base (const _Callable &)
    requires true
  {}
};

template<typename _Callable>
struct Derived :  Base<_Callable>
{
  using Base<_Callable>::Base;
};

template<typename _Callable>
Derived (_Callable) -> Derived<_Callable>;

inline Derived all = [] (auto&& __r) {};
