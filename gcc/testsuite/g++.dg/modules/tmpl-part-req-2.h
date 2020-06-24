
template<typename _Iterator, typename>
struct Trait;

template<typename _Iterator>
struct Trait<_Iterator, void> {};

template<typename _Iterator>
requires  true && true
struct Trait<_Iterator, void>
{
  template<typename _Iter> struct __cat {};
  
  template<typename _Iter> requires true struct __cat<_Iter> {};
};

template<typename _Iterator>
requires  true
struct Trait<_Iterator, void>
{
  template<typename _Iter> struct __diff {};
  
  template<typename _Iter> requires true struct __diff<_Iter> {};
};
