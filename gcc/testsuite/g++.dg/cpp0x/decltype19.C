// PR c++/42013

template<typename _Tp>
  _Tp
__attribute ((const)) declval();

template<typename _Tp, typename _Up>
  struct common_type
  {
    typedef __decltype(true  ? declval<_Tp>() : declval<_Up>()) typet;
    typedef __decltype(false ? declval<_Tp>() : declval<_Up>()) typef;
  };

template<typename, typename> struct is_same;

template<typename _Tp> struct is_same<_Tp, _Tp> { typedef _Tp type; };

void f()
{
  typedef common_type<int, const int>::typet typet;
  typedef common_type<int, const int>::typef typef;

  typedef is_same<typet, typef>::type type;
}
