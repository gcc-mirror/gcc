// { dg-do compile { target c++20 } }
// a potential fix for 99425 generated an ICE here.

template<typename _Tp>
struct is_nothrow_destructible;

template<typename _Tp>
struct common_reference;

template<typename _Tp>
concept same_as
  = true;

template<typename _Sent, typename _Iter>
concept sentinel_for
  = same_as<common_reference<_Sent>>
  && is_nothrow_destructible<_Iter>::value;

template<typename _Tp>
concept __member_end
  = requires (_Tp& __t)
  {
    { true }
    -> sentinel_for<decltype(__t)>;
  };

template<typename _Tp>
concept __adl_end
  = requires (_Tp& __t)
  {
    { true }
    -> sentinel_for<decltype(__t)>;
  };

template<typename _Tp>
requires __member_end<_Tp> || __adl_end<_Tp>
  void
  Bar (_Tp&& __t)
{
}

void test03 ()
{
  Bar (1); // { dg-error "no matching function" }
}
