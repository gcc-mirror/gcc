// PR c++/78006
// { dg-do compile { target c++14 } }

template<typename T> T&& declval() noexcept;

template<typename... _Tp>
  struct common_type;

template<typename _Tp>
  struct common_type<_Tp>
  { typedef _Tp type; };

template<typename _Tp, typename _Up>
  struct common_type<_Tp, _Up>
  { typedef decltype(true ? declval<_Tp>() : declval<_Up>()) type; };

template<typename _Tp, typename _Up, typename... _Vp>
  struct common_type<_Tp, _Up, _Vp...>
  {
    typedef typename
      common_type<typename common_type<_Tp, _Up>::type, _Vp...>::type type;
  };

template<typename... _Tp>
  using common_type_t = typename common_type<_Tp...>::type;

template <typename... TFs>
auto x(TFs&&... fs)
{
  using rt = common_type_t<decltype(fs(0))...>;    
  return [](auto) -> rt { };    
}

int main()
{
  x([](int){})(0);    
}
