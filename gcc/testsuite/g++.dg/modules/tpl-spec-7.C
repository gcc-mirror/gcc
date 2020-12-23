// { dg-additional-options "-fmodules-ts" }

export module foo;
// { dg-module-cmi foo }

typedef unsigned long size_t;

template<typename _Tp, bool _IsInt, bool _IsEnum>
class __make_unsigned_selector;

class __make_unsigned_selector_base
{
protected:
  template<typename...> struct _List { };

  template<typename _Tp, typename... _Up>
  struct _List<_Tp, _Up...> : _List<_Up...>
  { static constexpr size_t __size = sizeof(_Tp); };

  template<size_t _Sz, typename _Tp, bool = (_Sz <= _Tp::__size)>
  struct __select;
};

template<typename _Tp>
class __make_unsigned_selector<_Tp, false, true>
  : __make_unsigned_selector_base
{
  using _UInts = _List<unsigned char, unsigned short, unsigned int,
		       unsigned long, unsigned long long>;

  using __unsigned_type = typename __select<sizeof(_Tp), _UInts>::__type;
};






