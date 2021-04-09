template<typename T>
struct make_signed 
{
  using type = int;
};

template<typename S>
using make_signed_t = typename make_signed<S>::type;

template<typename U>
auto ssize (U &parm) -> make_signed_t<decltype(parm.call())>;
