
template<typename _Functor, typename _ArgTypes>
struct invoke_result;

template<typename _Fn, typename _ArgTypes>
struct is_invocable;

template<typename _Fn, typename... _Args>
concept invocable = is_invocable<_Fn, _Args...>::value;

template<typename _Fn, typename _Is>
requires invocable<_Fn, _Is>
  using indirect_result_t = typename invoke_result<_Fn, _Is>::type;

template<typename _Tp>
struct remove_cv;

template<typename _Iter, typename _Proj>
struct projected
{
  using value_type = remove_cv<indirect_result_t<_Proj&, _Iter>>;
};

