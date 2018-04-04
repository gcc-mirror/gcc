// PR c++/85118
// { dg-do compile { target c++14 } }

namespace std
{
  template<typename _Tp>
    struct remove_const
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_const<_Tp const>
    { typedef _Tp type; };


  template<typename _Tp>
    struct remove_volatile
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_volatile<_Tp volatile>
    { typedef _Tp type; };


  template<typename _Tp>
    struct remove_cv
    {
      typedef typename
      remove_const<typename remove_volatile<_Tp>::type>::type type;
    };

  template<typename _Tp>
    struct remove_reference
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp type; };

  template<typename _Tp>
    struct decay
    {
      using type = typename remove_reference<typename remove_const<_Tp>::type>::type;
    };

  template<typename _Tp>
    _Tp&&
    declval() noexcept;

  template<typename _Tp>
    constexpr _Tp&&
    forward(typename std::remove_reference<_Tp>::type& __t) noexcept
    { return static_cast<_Tp&&>(__t); }


  template<typename _Arg>
    struct _Mu
    {
      template<typename _CVArg, typename _Tuple>
         _CVArg&&
         operator()(_CVArg&& __arg, _Tuple&) const volatile
         { return std::forward<_CVArg>(__arg); }
    };

   template<typename _Functor, typename _Bound_args>
    struct _Bind
    {
      _Functor _M_f;
      _Bound_args _M_bound_args;

      template<typename _Args, typename _Result
         = decltype( std::declval<_Functor&>()(
               _Mu<_Bound_args>()( std::declval<_Bound_args&>(),
              std::declval<_Args&>() ) ) )>
         _Result
      operator()(_Args&& __args) { return {}; }

      template<typename _Args, typename _Result
         = decltype( std::declval<volatile _Functor&>()(
               _Mu<_Bound_args>()( std::declval<volatile _Bound_args&>(),
              std::declval<_Args&>() ) ) )>
         _Result
         operator()(_Args&& __args) volatile;

    };

  template<typename _Func, typename _BoundArgs>
    _Bind<typename decay<_Func>::type, typename decay<_BoundArgs>::type>
    bind(_Func&& __f, _BoundArgs&& __args)
    {
      return {
        std::forward<_Func>(__f),
          std::forward<_BoundArgs>(__args)
      };
    }

} // namespace std


template <typename T>
bool isOneOf(const T& )
{
    return false;
}

template <typename T, typename FirstType, typename... Tail>
bool isOneOf(const T& t, const FirstType& firstValue, const Tail&... tail)
{
    return t == firstValue || isOneOf(t, tail...);
}

int main()
{
    const auto isOneOfHelper = [](auto&&... params)
    {
      return isOneOf(std::forward<decltype(params)>(params)...);
    };

    auto isO = std::bind(isOneOfHelper, 'o');

    isO('o');
}
