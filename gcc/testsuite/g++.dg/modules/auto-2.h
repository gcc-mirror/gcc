
template<typename _Callable>
struct _RangeAdaptor
{
  constexpr _RangeAdaptor(const _Callable &) { }
};

template<typename _Callable>
_RangeAdaptor(_Callable) -> _RangeAdaptor<_Callable>;

template<unsigned _Nm>
inline constexpr _RangeAdaptor elements = [] (auto&& __r) {};

