// { dg-do compile }
// { dg-options "-fgnu-tm -O2" }

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
  using ::ptrdiff_t;
  using ::size_t;
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  void
  __throw_bad_exception(void) __attribute__((__noreturn__));
  void
  __throw_bad_alloc(void) __attribute__((__noreturn__));
  void
  __throw_bad_cast(void) __attribute__((__noreturn__));
  void
  __throw_bad_typeid(void) __attribute__((__noreturn__));
  void
  __throw_logic_error(const char*) __attribute__((__noreturn__));
  void
  __throw_domain_error(const char*) __attribute__((__noreturn__));
  void
  __throw_invalid_argument(const char*) __attribute__((__noreturn__));
  void
  __throw_length_error(const char*) __attribute__((__noreturn__));
  void
  __throw_out_of_range(const char*) __attribute__((__noreturn__));
  void
  __throw_runtime_error(const char*) __attribute__((__noreturn__));
  void
  __throw_range_error(const char*) __attribute__((__noreturn__));
  void
  __throw_overflow_error(const char*) __attribute__((__noreturn__));
  void
  __throw_underflow_error(const char*) __attribute__((__noreturn__));
  void
  __throw_ios_failure(const char*) __attribute__((__noreturn__));
  void
  __throw_system_error(int) __attribute__((__noreturn__));
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Iterator, typename _Container>
    class __normal_iterator;
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  struct __true_type { };
  struct __false_type { };
  template<bool>
    struct __truth_type
    { typedef __false_type __type; };
  template<>
    struct __truth_type<true>
    { typedef __true_type __type; };
  template<class _Sp, class _Tp>
    struct __traitor
    {
      enum { __value = bool(_Sp::__value) || bool(_Tp::__value) };
      typedef typename __truth_type<__value>::__type __type;
    };
  template<typename, typename>
    struct __are_same
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<typename _Tp>
    struct __are_same<_Tp, _Tp>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_void
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<>
    struct __is_void<void>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_integer
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<>
    struct __is_integer<bool>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<signed char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<unsigned char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<wchar_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<short>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<unsigned short>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<int>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<unsigned int>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<unsigned long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<long long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_integer<unsigned long long>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_floating
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<>
    struct __is_floating<float>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_floating<double>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_floating<long double>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_pointer
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<typename _Tp>
    struct __is_pointer<_Tp*>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_normal_iterator
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<typename _Iterator, typename _Container>
    struct __is_normal_iterator< __gnu_cxx::__normal_iterator<_Iterator,
	     _Container> >
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_arithmetic
    : public __traitor<__is_integer<_Tp>, __is_floating<_Tp> >
    { };
  template<typename _Tp>
    struct __is_fundamental
    : public __traitor<__is_void<_Tp>, __is_arithmetic<_Tp> >
    { };
  template<typename _Tp>
    struct __is_scalar
    : public __traitor<__is_arithmetic<_Tp>, __is_pointer<_Tp> >
    { };
  template<typename _Tp>
    struct __is_char
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<>
    struct __is_char<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_char<wchar_t>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_byte
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
  template<>
    struct __is_byte<char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_byte<signed char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<>
    struct __is_byte<unsigned char>
    {
      enum { __value = 1 };
      typedef __true_type __type;
    };
  template<typename _Tp>
    struct __is_move_iterator
    {
      enum { __value = 0 };
      typedef __false_type __type;
    };
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<bool, typename>
    struct __enable_if
    { };
  template<typename _Tp>
    struct __enable_if<true, _Tp>
    { typedef _Tp __type; };
  template<bool _Cond, typename _Iftrue, typename _Iffalse>
    struct __conditional_type
    { typedef _Iftrue __type; };
  template<typename _Iftrue, typename _Iffalse>
    struct __conditional_type<false, _Iftrue, _Iffalse>
    { typedef _Iffalse __type; };
  template<typename _Tp>
    struct __add_unsigned
    {
    private:
      typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
    public:
      typedef typename __if_type::__type __type;
    };
  template<>
    struct __add_unsigned<char>
    { typedef unsigned char __type; };
  template<>
    struct __add_unsigned<signed char>
    { typedef unsigned char __type; };
  template<>
    struct __add_unsigned<short>
    { typedef unsigned short __type; };
  template<>
    struct __add_unsigned<int>
    { typedef unsigned int __type; };
  template<>
    struct __add_unsigned<long>
    { typedef unsigned long __type; };
  template<>
    struct __add_unsigned<long long>
    { typedef unsigned long long __type; };
  template<>
    struct __add_unsigned<bool>;
  template<>
    struct __add_unsigned<wchar_t>;
  template<typename _Tp>
    struct __remove_unsigned
    {
    private:
      typedef __enable_if<std::__is_integer<_Tp>::__value, _Tp> __if_type;
    public:
      typedef typename __if_type::__type __type;
    };
  template<>
    struct __remove_unsigned<char>
    { typedef signed char __type; };
  template<>
    struct __remove_unsigned<unsigned char>
    { typedef signed char __type; };
  template<>
    struct __remove_unsigned<unsigned short>
    { typedef short __type; };
  template<>
    struct __remove_unsigned<unsigned int>
    { typedef int __type; };
  template<>
    struct __remove_unsigned<unsigned long>
    { typedef long __type; };
  template<>
    struct __remove_unsigned<unsigned long long>
    { typedef long long __type; };
  template<>
    struct __remove_unsigned<bool>;
  template<>
    struct __remove_unsigned<wchar_t>;
  template<typename _Type>
    inline bool
    __is_null_pointer(_Type* __ptr)
    { return __ptr == 0; }
  template<typename _Type>
    inline bool
    __is_null_pointer(_Type)
    { return false; }
  template<typename _Tp, bool = std::__is_integer<_Tp>::__value>
    struct __promote
    { typedef double __type; };
  template<typename _Tp>
    struct __promote<_Tp, false>
    { typedef _Tp __type; };
  template<typename _Tp, typename _Up>
    struct __promote_2
    {
    private:
      typedef typename __promote<_Tp>::__type __type1;
      typedef typename __promote<_Up>::__type __type2;
    public:
      typedef __typeof__(__type1() + __type2()) __type;
    };
  template<typename _Tp, typename _Up, typename _Vp>
    struct __promote_3
    {
    private:
      typedef typename __promote<_Tp>::__type __type1;
      typedef typename __promote<_Up>::__type __type2;
      typedef typename __promote<_Vp>::__type __type3;
    public:
      typedef __typeof__(__type1() + __type2() + __type3()) __type;
    };
  template<typename _Tp, typename _Up, typename _Vp, typename _Wp>
    struct __promote_4
    {
    private:
      typedef typename __promote<_Tp>::__type __type1;
      typedef typename __promote<_Up>::__type __type2;
      typedef typename __promote<_Vp>::__type __type3;
      typedef typename __promote<_Wp>::__type __type4;
    public:
      typedef __typeof__(__type1() + __type2() + __type3() + __type4()) __type;
    };
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Value>
    struct __numeric_traits_integer
    {
      static const _Value __min = (((_Value)(-1) < 0) ? (_Value)1 << (sizeof(_Value) * 8 - ((_Value)(-1) < 0)) : (_Value)0);
      static const _Value __max = (((_Value)(-1) < 0) ? (((((_Value)1 << ((sizeof(_Value) * 8 - ((_Value)(-1) < 0)) - 1)) - 1) << 1) + 1) : ~(_Value)0);
      static const bool __is_signed = ((_Value)(-1) < 0);
      static const int __digits = (sizeof(_Value) * 8 - ((_Value)(-1) < 0));
    };
  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__min;
  template<typename _Value>
    const _Value __numeric_traits_integer<_Value>::__max;
  template<typename _Value>
    const bool __numeric_traits_integer<_Value>::__is_signed;
  template<typename _Value>
    const int __numeric_traits_integer<_Value>::__digits;
  template<typename _Value>
    struct __numeric_traits_floating
    {
      static const int __max_digits10 = (2 + (std::__are_same<_Value, float>::__value ? 24 : std::__are_same<_Value, double>::__value ? 53 : 64) * 3010 / 10000);
      static const bool __is_signed = true;
      static const int __digits10 = (std::__are_same<_Value, float>::__value ? 6 : std::__are_same<_Value, double>::__value ? 15 : 18);
      static const int __max_exponent10 = (std::__are_same<_Value, float>::__value ? 38 : std::__are_same<_Value, double>::__value ? 308 : 4932);
    };
  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__max_digits10;
  template<typename _Value>
    const bool __numeric_traits_floating<_Value>::__is_signed;
  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__digits10;
  template<typename _Value>
    const int __numeric_traits_floating<_Value>::__max_exponent10;
  template<typename _Value>
    struct __numeric_traits
    : public __conditional_type<std::__is_integer<_Value>::__value,
    __numeric_traits_integer<_Value>,
    __numeric_traits_floating<_Value> >::__type
    { };
}


namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    inline void
    swap(_Tp& __a, _Tp& __b)
    {

      _Tp __tmp = (__a);
      __a = (__b);
      __b = (__tmp);
    }
  template<typename _Tp, size_t _Nm>
    inline void
    swap(_Tp (&__a)[_Nm], _Tp (&__b)[_Nm])
    {
      for (size_t __n = 0; __n < _Nm; ++__n)
 swap(__a[__n], __b[__n]);
    }
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<class _T1, class _T2>
    struct pair
    {
      typedef _T1 first_type;
      typedef _T2 second_type;
      _T1 first;
      _T2 second;
      pair()
      : first(), second() { }
      pair(const _T1& __a, const _T2& __b)
      : first(__a), second(__b) { }
      template<class _U1, class _U2>
	pair(const pair<_U1, _U2>& __p)
 : first(__p.first),
   second(__p.second) { }
    };
  template<class _T1, class _T2>
    inline bool
    operator==(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first == __y.first && __x.second == __y.second; }
  template<class _T1, class _T2>
    inline bool
    operator<(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __x.first < __y.first
      || (!(__y.first < __x.first) && __x.second < __y.second); }
  template<class _T1, class _T2>
    inline bool
    operator!=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x == __y); }
  template<class _T1, class _T2>
    inline bool
    operator>(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return __y < __x; }
  template<class _T1, class _T2>
    inline bool
    operator<=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__y < __x); }
  template<class _T1, class _T2>
    inline bool
    operator>=(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y)
    { return !(__x < __y); }
  template<class _T1, class _T2>
    inline pair<_T1, _T2>
    make_pair(_T1 __x, _T2 __y)
    { return pair<_T1, _T2>(__x, __y); }
}


namespace std __attribute__ ((__visibility__ ("default"))) {
  struct input_iterator_tag { };
  struct output_iterator_tag { };
  struct forward_iterator_tag : public input_iterator_tag { };
  struct bidirectional_iterator_tag : public forward_iterator_tag { };
  struct random_access_iterator_tag : public bidirectional_iterator_tag { };
  template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,
	   typename _Pointer = _Tp*, typename _Reference = _Tp&>
    struct iterator
    {
      typedef _Category iterator_category;
      typedef _Tp value_type;
      typedef _Distance difference_type;
      typedef _Pointer pointer;
      typedef _Reference reference;
    };
  template<typename _Iterator>
    struct iterator_traits
    {
      typedef typename _Iterator::iterator_category iterator_category;
      typedef typename _Iterator::value_type value_type;
      typedef typename _Iterator::difference_type difference_type;
      typedef typename _Iterator::pointer pointer;
      typedef typename _Iterator::reference reference;
    };
  template<typename _Tp>
    struct iterator_traits<_Tp*>
    {
      typedef random_access_iterator_tag iterator_category;
      typedef _Tp value_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef _Tp& reference;
    };
  template<typename _Tp>
    struct iterator_traits<const _Tp*>
    {
      typedef random_access_iterator_tag iterator_category;
      typedef _Tp value_type;
      typedef ptrdiff_t difference_type;
      typedef const _Tp* pointer;
      typedef const _Tp& reference;
    };
  template<typename _Iter>
    inline typename iterator_traits<_Iter>::iterator_category
    __iterator_category(const _Iter&)
    { return typename iterator_traits<_Iter>::iterator_category(); }
}

namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _InputIterator>
    inline typename iterator_traits<_InputIterator>::difference_type
    __distance(_InputIterator __first, _InputIterator __last,
	       input_iterator_tag)
    {

      typename iterator_traits<_InputIterator>::difference_type __n = 0;
      while (__first != __last)
 {
   ++__first;
   ++__n;
 }
      return __n;
    }
  template<typename _RandomAccessIterator>
    inline typename iterator_traits<_RandomAccessIterator>::difference_type
    __distance(_RandomAccessIterator __first, _RandomAccessIterator __last,
	       random_access_iterator_tag)
    {

      return __last - __first;
    }
  template<typename _InputIterator>
    inline typename iterator_traits<_InputIterator>::difference_type
    distance(_InputIterator __first, _InputIterator __last)
    {
      return std::__distance(__first, __last,
	std::__iterator_category(__first));
    }
  template<typename _InputIterator, typename _Distance>
    inline void
    __advance(_InputIterator& __i, _Distance __n, input_iterator_tag)
    {

      while (__n--)
 ++__i;
    }
  template<typename _BidirectionalIterator, typename _Distance>
    inline void
    __advance(_BidirectionalIterator& __i, _Distance __n,
       bidirectional_iterator_tag)
    {

      if (__n > 0)
	while (__n--)
   ++__i;
      else
	while (__n++)
   --__i;
    }
  template<typename _RandomAccessIterator, typename _Distance>
    inline void
    __advance(_RandomAccessIterator& __i, _Distance __n,
	      random_access_iterator_tag)
    {

      __i += __n;
    }
  template<typename _InputIterator, typename _Distance>
    inline void
    advance(_InputIterator& __i, _Distance __n)
    {
      typename iterator_traits<_InputIterator>::difference_type __d = __n;
      std::__advance(__i, __d, std::__iterator_category(__i));
    }
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Iterator>
    class reverse_iterator
    : public iterator<typename iterator_traits<_Iterator>::iterator_category,
	typename iterator_traits<_Iterator>::value_type,
	typename iterator_traits<_Iterator>::difference_type,
	typename iterator_traits<_Iterator>::pointer,
		      typename iterator_traits<_Iterator>::reference>
    {
    protected:
      _Iterator current;
    public:
      typedef _Iterator iterator_type;
      typedef typename iterator_traits<_Iterator>::difference_type
	      difference_type;
      typedef typename iterator_traits<_Iterator>::reference reference;
      typedef typename iterator_traits<_Iterator>::pointer pointer;
    public:
      reverse_iterator() : current() { }
      explicit
      reverse_iterator(iterator_type __x) : current(__x) { }
      reverse_iterator(const reverse_iterator& __x)
      : current(__x.current) { }
      template<typename _Iter>
	reverse_iterator(const reverse_iterator<_Iter>& __x)
 : current(__x.base()) { }
      iterator_type
      base() const
      { return current; }
      reference
      operator*() const
      {
 _Iterator __tmp = current;
 return *--__tmp;
      }
      pointer
      operator->() const
      { return &(operator*()); }
      reverse_iterator&
      operator++()
      {
 --current;
 return *this;
      }
      reverse_iterator
      operator++(int)
      {
 reverse_iterator __tmp = *this;
 --current;
 return __tmp;
      }
      reverse_iterator&
      operator--()
      {
 ++current;
 return *this;
      }
      reverse_iterator
      operator--(int)
      {
 reverse_iterator __tmp = *this;
 ++current;
 return __tmp;
      }
      reverse_iterator
      operator+(difference_type __n) const
      { return reverse_iterator(current - __n); }
      reverse_iterator&
      operator+=(difference_type __n)
      {
 current -= __n;
 return *this;
      }
      reverse_iterator
      operator-(difference_type __n) const
      { return reverse_iterator(current + __n); }
      reverse_iterator&
      operator-=(difference_type __n)
      {
 current += __n;
 return *this;
      }
      reference
      operator[](difference_type __n) const
      { return *(*this + __n); }
    };
  template<typename _Iterator>
    inline bool
    operator==(const reverse_iterator<_Iterator>& __x,
	const reverse_iterator<_Iterator>& __y)
    { return __x.base() == __y.base(); }
  template<typename _Iterator>
    inline bool
    operator<(const reverse_iterator<_Iterator>& __x,
       const reverse_iterator<_Iterator>& __y)
    { return __y.base() < __x.base(); }
  template<typename _Iterator>
    inline bool
    operator!=(const reverse_iterator<_Iterator>& __x,
	const reverse_iterator<_Iterator>& __y)
    { return !(__x == __y); }
  template<typename _Iterator>
    inline bool
    operator>(const reverse_iterator<_Iterator>& __x,
       const reverse_iterator<_Iterator>& __y)
    { return __y < __x; }
  template<typename _Iterator>
    inline bool
    operator<=(const reverse_iterator<_Iterator>& __x,
	const reverse_iterator<_Iterator>& __y)
    { return !(__y < __x); }
  template<typename _Iterator>
    inline bool
    operator>=(const reverse_iterator<_Iterator>& __x,
	const reverse_iterator<_Iterator>& __y)
    { return !(__x < __y); }
  template<typename _Iterator>
    inline typename reverse_iterator<_Iterator>::difference_type
    operator-(const reverse_iterator<_Iterator>& __x,
       const reverse_iterator<_Iterator>& __y)
    { return __y.base() - __x.base(); }
  template<typename _Iterator>
    inline reverse_iterator<_Iterator>
    operator+(typename reverse_iterator<_Iterator>::difference_type __n,
       const reverse_iterator<_Iterator>& __x)
    { return reverse_iterator<_Iterator>(__x.base() - __n); }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator==(const reverse_iterator<_IteratorL>& __x,
	const reverse_iterator<_IteratorR>& __y)
    { return __x.base() == __y.base(); }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator<(const reverse_iterator<_IteratorL>& __x,
       const reverse_iterator<_IteratorR>& __y)
    { return __y.base() < __x.base(); }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator!=(const reverse_iterator<_IteratorL>& __x,
	const reverse_iterator<_IteratorR>& __y)
    { return !(__x == __y); }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator>(const reverse_iterator<_IteratorL>& __x,
       const reverse_iterator<_IteratorR>& __y)
    { return __y < __x; }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator<=(const reverse_iterator<_IteratorL>& __x,
	const reverse_iterator<_IteratorR>& __y)
    { return !(__y < __x); }
  template<typename _IteratorL, typename _IteratorR>
    inline bool
    operator>=(const reverse_iterator<_IteratorL>& __x,
	const reverse_iterator<_IteratorR>& __y)
    { return !(__x < __y); }
  template<typename _IteratorL, typename _IteratorR>
    inline typename reverse_iterator<_IteratorL>::difference_type
    operator-(const reverse_iterator<_IteratorL>& __x,
       const reverse_iterator<_IteratorR>& __y)
    { return __y.base() - __x.base(); }
  template<typename _Container>
    class back_insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {
    protected:
      _Container* container;
    public:
      typedef _Container container_type;
      explicit
      back_insert_iterator(_Container& __x) : container(&__x) { }
      back_insert_iterator&
      operator=(typename _Container::const_reference __value)
      {
 container->push_back(__value);
 return *this;
      }
      back_insert_iterator&
      operator*()
      { return *this; }
      back_insert_iterator&
      operator++()
      { return *this; }
      back_insert_iterator
      operator++(int)
      { return *this; }
    };
  template<typename _Container>
    inline back_insert_iterator<_Container>
    back_inserter(_Container& __x)
    { return back_insert_iterator<_Container>(__x); }
  template<typename _Container>
    class front_insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {
    protected:
      _Container* container;
    public:
      typedef _Container container_type;
      explicit front_insert_iterator(_Container& __x) : container(&__x) { }
      front_insert_iterator&
      operator=(typename _Container::const_reference __value)
      {
 container->push_front(__value);
 return *this;
      }
      front_insert_iterator&
      operator*()
      { return *this; }
      front_insert_iterator&
      operator++()
      { return *this; }
      front_insert_iterator
      operator++(int)
      { return *this; }
    };
  template<typename _Container>
    inline front_insert_iterator<_Container>
    front_inserter(_Container& __x)
    { return front_insert_iterator<_Container>(__x); }
  template<typename _Container>
    class insert_iterator
    : public iterator<output_iterator_tag, void, void, void, void>
    {
    protected:
      _Container* container;
      typename _Container::iterator iter;
    public:
      typedef _Container container_type;
      insert_iterator(_Container& __x, typename _Container::iterator __i)
      : container(&__x), iter(__i) {}
      insert_iterator&
      operator=(typename _Container::const_reference __value)
      {
 iter = container->insert(iter, __value);
 ++iter;
 return *this;
      }
      insert_iterator&
      operator*()
      { return *this; }
      insert_iterator&
      operator++()
      { return *this; }
      insert_iterator&
      operator++(int)
      { return *this; }
    };
  template<typename _Container, typename _Iterator>
    inline insert_iterator<_Container>
    inserter(_Container& __x, _Iterator __i)
    {
      return insert_iterator<_Container>(__x,
      typename _Container::iterator(__i));
    }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  using std::iterator_traits;
  using std::iterator;
  template<typename _Iterator, typename _Container>
    class __normal_iterator
    {
    protected:
      _Iterator _M_current;
    public:
      typedef _Iterator iterator_type;
      typedef typename iterator_traits<_Iterator>::iterator_category
							     iterator_category;
      typedef typename iterator_traits<_Iterator>::value_type value_type;
      typedef typename iterator_traits<_Iterator>::difference_type
							     difference_type;
      typedef typename iterator_traits<_Iterator>::reference reference;
      typedef typename iterator_traits<_Iterator>::pointer pointer;
      __normal_iterator() : _M_current(_Iterator()) { }
      explicit
      __normal_iterator(const _Iterator& __i) : _M_current(__i) { }
      template<typename _Iter>
	__normal_iterator(const __normal_iterator<_Iter,
     typename __enable_if<
	      (std::__are_same<_Iter, typename _Container::pointer>::__value),
	_Container>::__type>& __i)
	: _M_current(__i.base()) { }
      reference
      operator*() const
      { return *_M_current; }
      pointer
      operator->() const
      { return _M_current; }
      __normal_iterator&
      operator++()
      {
 ++_M_current;
 return *this;
      }
      __normal_iterator
      operator++(int)
      { return __normal_iterator(_M_current++); }
      __normal_iterator&
      operator--()
      {
 --_M_current;
 return *this;
      }
      __normal_iterator
      operator--(int)
      { return __normal_iterator(_M_current--); }
      reference
      operator[](const difference_type& __n) const
      { return _M_current[__n]; }
      __normal_iterator&
      operator+=(const difference_type& __n)
      { _M_current += __n; return *this; }
      __normal_iterator
      operator+(const difference_type& __n) const
      { return __normal_iterator(_M_current + __n); }
      __normal_iterator&
      operator-=(const difference_type& __n)
      { _M_current -= __n; return *this; }
      __normal_iterator
      operator-(const difference_type& __n) const
      { return __normal_iterator(_M_current - __n); }
      const _Iterator&
      base() const
      { return _M_current; }
    };
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator==(const __normal_iterator<_IteratorL, _Container>& __lhs,
	const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() == __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator==(const __normal_iterator<_Iterator, _Container>& __lhs,
	const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() == __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator!=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() != __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator!=(const __normal_iterator<_Iterator, _Container>& __lhs,
	const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() != __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator<(const __normal_iterator<_IteratorL, _Container>& __lhs,
       const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() < __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator<(const __normal_iterator<_Iterator, _Container>& __lhs,
       const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() < __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator>(const __normal_iterator<_IteratorL, _Container>& __lhs,
       const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() > __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator>(const __normal_iterator<_Iterator, _Container>& __lhs,
       const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() > __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator<=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() <= __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator<=(const __normal_iterator<_Iterator, _Container>& __lhs,
	const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() <= __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline bool
    operator>=(const __normal_iterator<_IteratorL, _Container>& __lhs,
	const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() >= __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline bool
    operator>=(const __normal_iterator<_Iterator, _Container>& __lhs,
	const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() >= __rhs.base(); }
  template<typename _IteratorL, typename _IteratorR, typename _Container>
    inline typename __normal_iterator<_IteratorL, _Container>::difference_type
    operator-(const __normal_iterator<_IteratorL, _Container>& __lhs,
       const __normal_iterator<_IteratorR, _Container>& __rhs)
    { return __lhs.base() - __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline typename __normal_iterator<_Iterator, _Container>::difference_type
    operator-(const __normal_iterator<_Iterator, _Container>& __lhs,
       const __normal_iterator<_Iterator, _Container>& __rhs)
    { return __lhs.base() - __rhs.base(); }
  template<typename _Iterator, typename _Container>
    inline __normal_iterator<_Iterator, _Container>
    operator+(typename __normal_iterator<_Iterator, _Container>::difference_type
       __n, const __normal_iterator<_Iterator, _Container>& __i)
    { return __normal_iterator<_Iterator, _Container>(__i.base() + __n); }
}
namespace std
{
  namespace __debug { }
}
namespace __gnu_debug
{
  using namespace std::__debug;
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<bool _BoolType>
    struct __iter_swap
    {
      template<typename _ForwardIterator1, typename _ForwardIterator2>
	static void
	iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
	{
	  typedef typename iterator_traits<_ForwardIterator1>::value_type
	    _ValueType1;
	  _ValueType1 __tmp = (*__a);
	  *__a = (*__b);
	  *__b = (__tmp);
 }
    };
  template<>
    struct __iter_swap<true>
    {
      template<typename _ForwardIterator1, typename _ForwardIterator2>
	static void
	iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
	{
	  swap(*__a, *__b);
	}
    };
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    inline void
    iter_swap(_ForwardIterator1 __a, _ForwardIterator2 __b)
    {
      typedef typename iterator_traits<_ForwardIterator1>::value_type
 _ValueType1;
      typedef typename iterator_traits<_ForwardIterator2>::value_type
 _ValueType2;




      typedef typename iterator_traits<_ForwardIterator1>::reference
 _ReferenceType1;
      typedef typename iterator_traits<_ForwardIterator2>::reference
 _ReferenceType2;
      std::__iter_swap<__are_same<_ValueType1, _ValueType2>::__value
 && __are_same<_ValueType1&, _ReferenceType1>::__value
 && __are_same<_ValueType2&, _ReferenceType2>::__value>::
 iter_swap(__a, __b);
    }
  template<typename _ForwardIterator1, typename _ForwardIterator2>
    _ForwardIterator2
    swap_ranges(_ForwardIterator1 __first1, _ForwardIterator1 __last1,
  _ForwardIterator2 __first2)
    {


      ;
      for (; __first1 != __last1; ++__first1, ++__first2)
 std::iter_swap(__first1, __first2);
      return __first2;
    }
  template<typename _Tp>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b)
    {

      if (__b < __a)
 return __b;
      return __a;
    }
  template<typename _Tp>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b)
    {

      if (__a < __b)
 return __b;
      return __a;
    }
  template<typename _Tp, typename _Compare>
    inline const _Tp&
    min(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      if (__comp(__b, __a))
 return __b;
      return __a;
    }
  template<typename _Tp, typename _Compare>
    inline const _Tp&
    max(const _Tp& __a, const _Tp& __b, _Compare __comp)
    {
      if (__comp(__a, __b))
 return __b;
      return __a;
    }
  template<typename _Iterator,
    bool _IsNormal = __is_normal_iterator<_Iterator>::__value>
    struct __niter_base
    {
      static _Iterator
      __b(_Iterator __it)
      { return __it; }
    };
  template<typename _Iterator>
    struct __niter_base<_Iterator, true>
    {
      static typename _Iterator::iterator_type
      __b(_Iterator __it)
      { return __it.base(); }
    };
  template<typename _Iterator,
    bool _IsMove = __is_move_iterator<_Iterator>::__value>
    struct __miter_base
    {
      static _Iterator
      __b(_Iterator __it)
      { return __it; }
    };
  template<typename _Iterator>
    struct __miter_base<_Iterator, true>
    {
      static typename _Iterator::iterator_type
      __b(_Iterator __it)
      { return __it.base(); }
    };
  template<bool, bool, typename>
    struct __copy_move
    {
      template<typename _II, typename _OI>
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
   for (; __first != __last; ++__result, ++__first)
     *__result = *__first;
   return __result;
 }
    };
  template<>
    struct __copy_move<false, false, random_access_iterator_tag>
    {
      template<typename _II, typename _OI>
	static _OI
	__copy_m(_II __first, _II __last, _OI __result)
	{
   typedef typename iterator_traits<_II>::difference_type _Distance;
   for(_Distance __n = __last - __first; __n > 0; --__n)
     {
       *__result = *__first;
       ++__first;
       ++__result;
     }
   return __result;
 }
    };
  template<bool _IsMove>
    struct __copy_move<_IsMove, true, random_access_iterator_tag>
    {
      template<typename _Tp>
	static _Tp*
	__copy_m(const _Tp* __first, const _Tp* __last, _Tp* __result)
	{
   __builtin_memmove(__result, __first,
       sizeof(_Tp) * (__last - __first));
   return __result + (__last - __first);
 }
    };
  template<bool _IsMove, typename _II, typename _OI>
    inline _OI
    __copy_move_a(_II __first, _II __last, _OI __result)
    {
      typedef typename iterator_traits<_II>::value_type _ValueTypeI;
      typedef typename iterator_traits<_OI>::value_type _ValueTypeO;
      typedef typename iterator_traits<_II>::iterator_category _Category;
      const bool __simple = (__is_pod(_ValueTypeI)
		      && __is_pointer<_II>::__value
		      && __is_pointer<_OI>::__value
	&& __are_same<_ValueTypeI, _ValueTypeO>::__value);
      return std::__copy_move<_IsMove, __simple,
		       _Category>::__copy_m(__first, __last, __result);
    }
  template<typename _CharT>
    struct char_traits;
  template<typename _CharT, typename _Traits>
    class istreambuf_iterator;
  template<typename _CharT, typename _Traits>
    class ostreambuf_iterator;
  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
      ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(_CharT*, _CharT*,
     ostreambuf_iterator<_CharT, char_traits<_CharT> >);
  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
      ostreambuf_iterator<_CharT, char_traits<_CharT> > >::__type
    __copy_move_a2(const _CharT*, const _CharT*,
     ostreambuf_iterator<_CharT, char_traits<_CharT> >);
  template<bool _IsMove, typename _CharT>
    typename __gnu_cxx::__enable_if<__is_char<_CharT>::__value,
	_CharT*>::__type
    __copy_move_a2(istreambuf_iterator<_CharT, char_traits<_CharT> >,
     istreambuf_iterator<_CharT, char_traits<_CharT> >, _CharT*);
  template<bool _IsMove, typename _II, typename _OI>
    inline _OI
    __copy_move_a2(_II __first, _II __last, _OI __result)
    {
      return _OI(std::__copy_move_a<_IsMove>
   (std::__niter_base<_II>::__b(__first),
    std::__niter_base<_II>::__b(__last),
    std::__niter_base<_OI>::__b(__result)));
    }
  template<typename _II, typename _OI>
    inline _OI
    copy(_II __first, _II __last, _OI __result)
    {


      ;
      return (std::__copy_move_a2<__is_move_iterator<_II>::__value>
       (std::__miter_base<_II>::__b(__first),
	std::__miter_base<_II>::__b(__last), __result));
    }
  template<bool, bool, typename>
    struct __copy_move_backward
    {
      template<typename _BI1, typename _BI2>
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
   while (__first != __last)
     *--__result = *--__last;
   return __result;
 }
    };
  template<>
    struct __copy_move_backward<false, false, random_access_iterator_tag>
    {
      template<typename _BI1, typename _BI2>
	static _BI2
	__copy_move_b(_BI1 __first, _BI1 __last, _BI2 __result)
	{
   typename iterator_traits<_BI1>::difference_type __n;
   for (__n = __last - __first; __n > 0; --__n)
     *--__result = *--__last;
   return __result;
 }
    };
  template<bool _IsMove>
    struct __copy_move_backward<_IsMove, true, random_access_iterator_tag>
    {
      template<typename _Tp>
	static _Tp*
	__copy_move_b(const _Tp* __first, const _Tp* __last, _Tp* __result)
	{
   const ptrdiff_t _Num = __last - __first;
   __builtin_memmove(__result - _Num, __first, sizeof(_Tp) * _Num);
   return __result - _Num;
 }
    };
  template<bool _IsMove, typename _BI1, typename _BI2>
    inline _BI2
    __copy_move_backward_a(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      typedef typename iterator_traits<_BI1>::value_type _ValueType1;
      typedef typename iterator_traits<_BI2>::value_type _ValueType2;
      typedef typename iterator_traits<_BI1>::iterator_category _Category;
      const bool __simple = (__is_pod(_ValueType1)
		      && __is_pointer<_BI1>::__value
		      && __is_pointer<_BI2>::__value
	&& __are_same<_ValueType1, _ValueType2>::__value);
      return std::__copy_move_backward<_IsMove, __simple,
				_Category>::__copy_move_b(__first,
	 __last,
	 __result);
    }
  template<bool _IsMove, typename _BI1, typename _BI2>
    inline _BI2
    __copy_move_backward_a2(_BI1 __first, _BI1 __last, _BI2 __result)
    {
      return _BI2(std::__copy_move_backward_a<_IsMove>
    (std::__niter_base<_BI1>::__b(__first),
     std::__niter_base<_BI1>::__b(__last),
     std::__niter_base<_BI2>::__b(__result)));
    }
  template<typename _BI1, typename _BI2>
    inline _BI2
    copy_backward(_BI1 __first, _BI1 __last, _BI2 __result)
    {



      ;
      return (std::__copy_move_backward_a2<__is_move_iterator<_BI1>::__value>
       (std::__miter_base<_BI1>::__b(__first),
	std::__miter_base<_BI1>::__b(__last), __result));
    }
  template<typename _ForwardIterator, typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<!__is_scalar<_Tp>::__value, void>::__type
    __fill_a(_ForwardIterator __first, _ForwardIterator __last,
       const _Tp& __value)
    {
      for (; __first != __last; ++__first)
 *__first = __value;
    }
  template<typename _ForwardIterator, typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<__is_scalar<_Tp>::__value, void>::__type
    __fill_a(_ForwardIterator __first, _ForwardIterator __last,
      const _Tp& __value)
    {
      const _Tp __tmp = __value;
      for (; __first != __last; ++__first)
 *__first = __tmp;
    }
  template<typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<__is_byte<_Tp>::__value, void>::__type
    __fill_a(_Tp* __first, _Tp* __last, const _Tp& __c)
    {
      const _Tp __tmp = __c;
      __builtin_memset(__first, static_cast<unsigned char>(__tmp),
	 __last - __first);
    }
  template<typename _ForwardIterator, typename _Tp>
    inline void
    fill(_ForwardIterator __first, _ForwardIterator __last, const _Tp& __value)
    {

      ;
      std::__fill_a(std::__niter_base<_ForwardIterator>::__b(__first),
      std::__niter_base<_ForwardIterator>::__b(__last), __value);
    }
  template<typename _OutputIterator, typename _Size, typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<!__is_scalar<_Tp>::__value, _OutputIterator>::__type
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      for (; __n > 0; --__n, ++__first)
 *__first = __value;
      return __first;
    }
  template<typename _OutputIterator, typename _Size, typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<__is_scalar<_Tp>::__value, _OutputIterator>::__type
    __fill_n_a(_OutputIterator __first, _Size __n, const _Tp& __value)
    {
      const _Tp __tmp = __value;
      for (; __n > 0; --__n, ++__first)
 *__first = __tmp;
      return __first;
    }
  template<typename _Size, typename _Tp>
    inline typename
    __gnu_cxx::__enable_if<__is_byte<_Tp>::__value, _Tp*>::__type
    __fill_n_a(_Tp* __first, _Size __n, const _Tp& __c)
    {
      std::__fill_a(__first, __first + __n, __c);
      return __first + __n;
    }
  template<typename _OI, typename _Size, typename _Tp>
    inline _OI
    fill_n(_OI __first, _Size __n, const _Tp& __value)
    {

      return _OI(std::__fill_n_a(std::__niter_base<_OI>::__b(__first),
     __n, __value));
    }
  template<bool _BoolType>
    struct __equal
    {
      template<typename _II1, typename _II2>
	static bool
	equal(_II1 __first1, _II1 __last1, _II2 __first2)
	{
   for (; __first1 != __last1; ++__first1, ++__first2)
     if (!(*__first1 == *__first2))
       return false;
   return true;
 }
    };
  template<>
    struct __equal<true>
    {
      template<typename _Tp>
	static bool
	equal(const _Tp* __first1, const _Tp* __last1, const _Tp* __first2)
	{
   return !__builtin_memcmp(__first1, __first2, sizeof(_Tp)
       * (__last1 - __first1));
 }
    };
  template<typename _II1, typename _II2>
    inline bool
    __equal_aux(_II1 __first1, _II1 __last1, _II2 __first2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
      const bool __simple = (__is_integer<_ValueType1>::__value
		      && __is_pointer<_II1>::__value
		      && __is_pointer<_II2>::__value
	&& __are_same<_ValueType1, _ValueType2>::__value);
      return std::__equal<__simple>::equal(__first1, __last1, __first2);
    }
  template<typename, typename>
    struct __lc_rai
    {
      template<typename _II1, typename _II2>
	static _II1
	__newlast1(_II1, _II1 __last1, _II2, _II2)
	{ return __last1; }
      template<typename _II>
	static bool
	__cnd2(_II __first, _II __last)
	{ return __first != __last; }
    };
  template<>
    struct __lc_rai<random_access_iterator_tag, random_access_iterator_tag>
    {
      template<typename _RAI1, typename _RAI2>
	static _RAI1
	__newlast1(_RAI1 __first1, _RAI1 __last1,
     _RAI2 __first2, _RAI2 __last2)
	{
   const typename iterator_traits<_RAI1>::difference_type
     __diff1 = __last1 - __first1;
   const typename iterator_traits<_RAI2>::difference_type
     __diff2 = __last2 - __first2;
   return __diff2 < __diff1 ? __first1 + __diff2 : __last1;
 }
      template<typename _RAI>
	static bool
	__cnd2(_RAI, _RAI)
	{ return true; }
    };
  template<bool _BoolType>
    struct __lexicographical_compare
    {
      template<typename _II1, typename _II2>
	static bool __lc(_II1, _II1, _II2, _II2);
    };
  template<bool _BoolType>
    template<typename _II1, typename _II2>
      bool
      __lexicographical_compare<_BoolType>::
      __lc(_II1 __first1, _II1 __last1, _II2 __first2, _II2 __last2)
      {
 typedef typename iterator_traits<_II1>::iterator_category _Category1;
 typedef typename iterator_traits<_II2>::iterator_category _Category2;
 typedef std::__lc_rai<_Category1, _Category2> __rai_type;
 __last1 = __rai_type::__newlast1(__first1, __last1,
      __first2, __last2);
 for (; __first1 != __last1 && __rai_type::__cnd2(__first2, __last2);
      ++__first1, ++__first2)
   {
     if (*__first1 < *__first2)
       return true;
     if (*__first2 < *__first1)
       return false;
   }
 return __first1 == __last1 && __first2 != __last2;
      }
  template<>
    struct __lexicographical_compare<true>
    {
      template<typename _Tp, typename _Up>
	static bool
	__lc(const _Tp* __first1, const _Tp* __last1,
      const _Up* __first2, const _Up* __last2)
 {
   const size_t __len1 = __last1 - __first1;
   const size_t __len2 = __last2 - __first2;
   const int __result = __builtin_memcmp(__first1, __first2,
      std::min(__len1, __len2));
   return __result != 0 ? __result < 0 : __len1 < __len2;
 }
    };
  template<typename _II1, typename _II2>
    inline bool
    __lexicographical_compare_aux(_II1 __first1, _II1 __last1,
      _II2 __first2, _II2 __last2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;
      const bool __simple =
 (__is_byte<_ValueType1>::__value && __is_byte<_ValueType2>::__value
  && !__gnu_cxx::__numeric_traits<_ValueType1>::__is_signed
  && !__gnu_cxx::__numeric_traits<_ValueType2>::__is_signed
  && __is_pointer<_II1>::__value
  && __is_pointer<_II2>::__value);
      return std::__lexicographical_compare<__simple>::__lc(__first1, __last1,
	   __first2, __last2);
    }
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _II1, typename _II2>
    inline bool
    equal(_II1 __first1, _II1 __last1, _II2 __first2)
    {



      ;
      return std::__equal_aux(std::__niter_base<_II1>::__b(__first1),
	 std::__niter_base<_II1>::__b(__last1),
	 std::__niter_base<_II2>::__b(__first2));
    }
  template<typename _IIter1, typename _IIter2, typename _BinaryPredicate>
    inline bool
    equal(_IIter1 __first1, _IIter1 __last1,
   _IIter2 __first2, _BinaryPredicate __binary_pred)
    {


      ;
      for (; __first1 != __last1; ++__first1, ++__first2)
 if (!bool(__binary_pred(*__first1, *__first2)))
   return false;
      return true;
    }
  template<typename _II1, typename _II2>
    inline bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
       _II2 __first2, _II2 __last2)
    {
      typedef typename iterator_traits<_II1>::value_type _ValueType1;
      typedef typename iterator_traits<_II2>::value_type _ValueType2;




      ;
      ;
      return std::__lexicographical_compare_aux
 (std::__niter_base<_II1>::__b(__first1),
  std::__niter_base<_II1>::__b(__last1),
  std::__niter_base<_II2>::__b(__first2),
  std::__niter_base<_II2>::__b(__last2));
    }
  template<typename _II1, typename _II2, typename _Compare>
    bool
    lexicographical_compare(_II1 __first1, _II1 __last1,
       _II2 __first2, _II2 __last2, _Compare __comp)
    {
      typedef typename iterator_traits<_II1>::iterator_category _Category1;
      typedef typename iterator_traits<_II2>::iterator_category _Category2;
      typedef std::__lc_rai<_Category1, _Category2> __rai_type;


      ;
      ;
      __last1 = __rai_type::__newlast1(__first1, __last1, __first2, __last2);
      for (; __first1 != __last1 && __rai_type::__cnd2(__first2, __last2);
    ++__first1, ++__first2)
 {
   if (__comp(*__first1, *__first2))
     return true;
   if (__comp(*__first2, *__first1))
     return false;
 }
      return __first1 == __last1 && __first2 != __last2;
    }
  template<typename _InputIterator1, typename _InputIterator2>
    pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
      _InputIterator2 __first2)
    {



      ;
      while (__first1 != __last1 && *__first1 == *__first2)
	{
   ++__first1;
   ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }
  template<typename _InputIterator1, typename _InputIterator2,
    typename _BinaryPredicate>
    pair<_InputIterator1, _InputIterator2>
    mismatch(_InputIterator1 __first1, _InputIterator1 __last1,
      _InputIterator2 __first2, _BinaryPredicate __binary_pred)
    {


      ;
      while (__first1 != __last1 && bool(__binary_pred(*__first1, *__first2)))
	{
   ++__first1;
   ++__first2;
	}
      return pair<_InputIterator1, _InputIterator2>(__first1, __first2);
    }
}

extern "C++" {
namespace std
{
  class exception
  {
  public:
    exception() throw() { }
    virtual ~exception() throw();
    virtual const char* what() const throw();
  };
  class bad_exception : public exception
  {
  public:
    bad_exception() throw() { }
    virtual ~bad_exception() throw();
    virtual const char* what() const throw();
  };
  typedef void (*terminate_handler) ();
  typedef void (*unexpected_handler) ();
  terminate_handler set_terminate(terminate_handler) throw();
  void terminate() __attribute__ ((__noreturn__));
  unexpected_handler set_unexpected(unexpected_handler) throw();
  void unexpected() __attribute__ ((__noreturn__));
  bool uncaught_exception() throw();
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  void __verbose_terminate_handler();
}
}
extern "C++" {
namespace std
{
  class bad_alloc : public exception
  {
  public:
    bad_alloc() throw() { }
    virtual ~bad_alloc() throw();
    virtual const char* what() const throw();
  };
  struct nothrow_t { };
  extern const nothrow_t nothrow;
  typedef void (*new_handler)();
  new_handler set_new_handler(new_handler) throw();
}
void* operator new(std::size_t)
#if __cplusplus <= 201402L
throw (std::bad_alloc)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
;
void* operator new[](std::size_t)
#if __cplusplus <= 201402L
throw (std::bad_alloc)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
;
void operator delete(void*) throw();
void operator delete[](void*) throw();
void* operator new(std::size_t, const std::nothrow_t&) throw();
void* operator new[](std::size_t, const std::nothrow_t&) throw();
void operator delete(void*, const std::nothrow_t&) throw();
void operator delete[](void*, const std::nothrow_t&) throw();
inline void* operator new(std::size_t, void* __p) throw() { return __p; }
inline void* operator new[](std::size_t, void* __p) throw() { return __p; }
inline void operator delete (void*, void*) throw() { }
inline void operator delete[](void*, void*) throw() { }
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  using std::size_t;
  using std::ptrdiff_t;
  template<typename _Tp>
    class new_allocator
    {
    public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp& reference;
      typedef const _Tp& const_reference;
      typedef _Tp value_type;
      template<typename _Tp1>
	struct rebind
	{ typedef new_allocator<_Tp1> other; };
      new_allocator() throw() { }
      new_allocator(const new_allocator&) throw() { }
      template<typename _Tp1>
	new_allocator(const new_allocator<_Tp1>&) throw() { }
      ~new_allocator() throw() { }
      pointer
      address(reference __x) const { return &__x; }
      const_pointer
      address(const_reference __x) const { return &__x; }
      pointer
      allocate(size_type __n, const void* = 0)
      {
 if (__builtin_expect(__n > this->max_size(), false))
   std::__throw_bad_alloc();
 return static_cast<_Tp*>(::operator new(__n * sizeof(_Tp)));
      }
      void
      deallocate(pointer __p, size_type)
      { ::operator delete(__p); }
      size_type
      max_size() const throw()
      { return size_t(-1) / sizeof(_Tp); }
      void
      construct(pointer __p, const _Tp& __val)
      { ::new((void *)__p) _Tp(__val); }
      void
      destroy(pointer __p) { __p->~_Tp(); }
    };
  template<typename _Tp>
    inline bool
    operator==(const new_allocator<_Tp>&, const new_allocator<_Tp>&)
    { return true; }
  template<typename _Tp>
    inline bool
    operator!=(const new_allocator<_Tp>&, const new_allocator<_Tp>&)
    { return false; }
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    class allocator;
  template<>
    class allocator<void>
    {
    public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef void* pointer;
      typedef const void* const_pointer;
      typedef void value_type;
      template<typename _Tp1>
	struct rebind
	{ typedef allocator<_Tp1> other; };
    };
  template<typename _Tp>
    class allocator: public __gnu_cxx::new_allocator<_Tp>
    {
   public:
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Tp* pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp& reference;
      typedef const _Tp& const_reference;
      typedef _Tp value_type;
      template<typename _Tp1>
	struct rebind
	{ typedef allocator<_Tp1> other; };
      allocator() throw() { }
      allocator(const allocator& __a) throw()
      : __gnu_cxx::new_allocator<_Tp>(__a) { }
      template<typename _Tp1>
	allocator(const allocator<_Tp1>&) throw() { }
      ~allocator() throw() { }
    };
  template<typename _T1, typename _T2>
    inline bool
    operator==(const allocator<_T1>&, const allocator<_T2>&)
    { return true; }
  template<typename _Tp>
    inline bool
    operator==(const allocator<_Tp>&, const allocator<_Tp>&)
    { return true; }
  template<typename _T1, typename _T2>
    inline bool
    operator!=(const allocator<_T1>&, const allocator<_T2>&)
    { return false; }
  template<typename _Tp>
    inline bool
    operator!=(const allocator<_Tp>&, const allocator<_Tp>&)
    { return false; }
  extern template class allocator<char>;
  extern template class allocator<wchar_t>;
  template<typename _Alloc, bool = __is_empty(_Alloc)>
    struct __alloc_swap
    { static void _S_do_it(_Alloc&, _Alloc&) { } };
  template<typename _Alloc>
    struct __alloc_swap<_Alloc, false>
    {
      static void
      _S_do_it(_Alloc& __one, _Alloc& __two)
      {
 if (__one != __two)
   swap(__one, __two);
      }
    };
  template<typename _Alloc, bool = __is_empty(_Alloc)>
    struct __alloc_neq
    {
      static bool
      _S_do_it(const _Alloc&, const _Alloc&)
      { return false; }
    };
  template<typename _Alloc>
    struct __alloc_neq<_Alloc, false>
    {
      static bool
      _S_do_it(const _Alloc& __one, const _Alloc& __two)
      { return __one != __two; }
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  struct _List_node_base
  {
    _List_node_base* _M_next;
    _List_node_base* _M_prev;
    static void
    swap(_List_node_base& __x, _List_node_base& __y);
    void
    transfer(_List_node_base * const __first,
      _List_node_base * const __last);
    void
    reverse();
    void
    hook(_List_node_base * const __position);
    void
    unhook();
  };
  template<typename _Tp>
    struct _List_node : public _List_node_base
    {
      _Tp _M_data;
    };
  template<typename _Tp>
    struct _List_iterator
    {
      typedef _List_iterator<_Tp> _Self;
      typedef _List_node<_Tp> _Node;
      typedef ptrdiff_t difference_type;
      typedef std::bidirectional_iterator_tag iterator_category;
      typedef _Tp value_type;
      typedef _Tp* pointer;
      typedef _Tp& reference;
      _List_iterator()
      : _M_node() { }
      explicit
      _List_iterator(_List_node_base* __x)
      : _M_node(__x) { }
      reference
      operator*() const
      { return static_cast<_Node*>(_M_node)->_M_data; }
      pointer
      operator->() const
      { return &static_cast<_Node*>(_M_node)->_M_data; }
      _Self&
      operator++()
      {
 _M_node = _M_node->_M_next;
 return *this;
      }
      _Self
      operator++(int)
      {
 _Self __tmp = *this;
 _M_node = _M_node->_M_next;
 return __tmp;
      }
      _Self&
      operator--()
      {
 _M_node = _M_node->_M_prev;
 return *this;
      }
      _Self
      operator--(int)
      {
 _Self __tmp = *this;
 _M_node = _M_node->_M_prev;
 return __tmp;
      }
      bool
      operator==(const _Self& __x) const
      { return _M_node == __x._M_node; }
      bool
      operator!=(const _Self& __x) const
      { return _M_node != __x._M_node; }
      _List_node_base* _M_node;
    };
  template<typename _Tp>
    struct _List_const_iterator
    {
      typedef _List_const_iterator<_Tp> _Self;
      typedef const _List_node<_Tp> _Node;
      typedef _List_iterator<_Tp> iterator;
      typedef ptrdiff_t difference_type;
      typedef std::bidirectional_iterator_tag iterator_category;
      typedef _Tp value_type;
      typedef const _Tp* pointer;
      typedef const _Tp& reference;
      _List_const_iterator()
      : _M_node() { }
      explicit
      _List_const_iterator(const _List_node_base* __x)
      : _M_node(__x) { }
      _List_const_iterator(const iterator& __x)
      : _M_node(__x._M_node) { }
      reference
      operator*() const
      { return static_cast<_Node*>(_M_node)->_M_data; }
      pointer
      operator->() const
      { return &static_cast<_Node*>(_M_node)->_M_data; }
      _Self&
      operator++()
      {
 _M_node = _M_node->_M_next;
 return *this;
      }
      _Self
      operator++(int)
      {
 _Self __tmp = *this;
 _M_node = _M_node->_M_next;
 return __tmp;
      }
      _Self&
      operator--()
      {
 _M_node = _M_node->_M_prev;
 return *this;
      }
      _Self
      operator--(int)
      {
 _Self __tmp = *this;
 _M_node = _M_node->_M_prev;
 return __tmp;
      }
      bool
      operator==(const _Self& __x) const
      { return _M_node == __x._M_node; }
      bool
      operator!=(const _Self& __x) const
      { return _M_node != __x._M_node; }
      const _List_node_base* _M_node;
    };
  template<typename _Val>
    inline bool
    operator==(const _List_iterator<_Val>& __x,
	const _List_const_iterator<_Val>& __y)
    { return __x._M_node == __y._M_node; }
  template<typename _Val>
    inline bool
    operator!=(const _List_iterator<_Val>& __x,
	       const _List_const_iterator<_Val>& __y)
    { return __x._M_node != __y._M_node; }
  template<typename _Tp, typename _Alloc>
    class _List_base
    {
    protected:
      typedef typename _Alloc::template rebind<_List_node<_Tp> >::other
	_Node_alloc_type;
      typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;
      struct _List_impl
      : public _Node_alloc_type
      {
 _List_node_base _M_node;
 _List_impl()
 : _Node_alloc_type(), _M_node()
 { }
 _List_impl(const _Node_alloc_type& __a)
 : _Node_alloc_type(__a), _M_node()
 { }
      };
      _List_impl _M_impl;
      _List_node<_Tp>*
      _M_get_node()
      { return _M_impl._Node_alloc_type::allocate(1); }
      void
      _M_put_node(_List_node<_Tp>* __p)
      { _M_impl._Node_alloc_type::deallocate(__p, 1); }
  public:
      typedef _Alloc allocator_type;
      _Node_alloc_type&
      _M_get_Node_allocator()
      { return *static_cast<_Node_alloc_type*>(&this->_M_impl); }
      const _Node_alloc_type&
      _M_get_Node_allocator() const
      { return *static_cast<const _Node_alloc_type*>(&this->_M_impl); }
      _Tp_alloc_type
      _M_get_Tp_allocator() const
      { return _Tp_alloc_type(_M_get_Node_allocator()); }
      allocator_type
      get_allocator() const
      { return allocator_type(_M_get_Node_allocator()); }
      _List_base()
      : _M_impl()
      { _M_init(); }
      _List_base(const allocator_type& __a)
      : _M_impl(__a)
      { _M_init(); }
      ~_List_base()
      { _M_clear(); }
      void
      _M_clear();
      void
      _M_init()
      {
	this->_M_impl._M_node._M_next = &this->_M_impl._M_node;
	this->_M_impl._M_node._M_prev = &this->_M_impl._M_node;
      }
    };
  template<typename _Tp, typename _Alloc = std::allocator<_Tp> >
    class list : protected _List_base<_Tp, _Alloc>
    {
      typedef typename _Alloc::value_type _Alloc_value_type;


      typedef _List_base<_Tp, _Alloc> _Base;
      typedef typename _Base::_Tp_alloc_type _Tp_alloc_type;
    public:
      typedef _Tp value_type;
      typedef typename _Tp_alloc_type::pointer pointer;
      typedef typename _Tp_alloc_type::const_pointer const_pointer;
      typedef typename _Tp_alloc_type::reference reference;
      typedef typename _Tp_alloc_type::const_reference const_reference;
      typedef _List_iterator<_Tp> iterator;
      typedef _List_const_iterator<_Tp> const_iterator;
      typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
      typedef std::reverse_iterator<iterator> reverse_iterator;
      typedef size_t size_type;
      typedef ptrdiff_t difference_type;
      typedef _Alloc allocator_type;
    protected:
      typedef _List_node<_Tp> _Node;
      using _Base::_M_impl;
      using _Base::_M_put_node;
      using _Base::_M_get_node;
      using _Base::_M_get_Tp_allocator;
      using _Base::_M_get_Node_allocator;
      _Node*
      _M_create_node(const value_type& __x)
      {
 _Node* __p = this->_M_get_node();
 try
   {
     _M_get_Tp_allocator().construct(&__p->_M_data, __x);
   }
 catch(...)
   {
     _M_put_node(__p);
     throw;
   }
 return __p;
      }
    public:
      list()
      : _Base() { }
      explicit
      list(const allocator_type& __a)
      : _Base(__a) { }
      explicit
      list(size_type __n, const value_type& __value = value_type(),
    const allocator_type& __a = allocator_type())
      : _Base(__a)
      { _M_fill_initialize(__n, __value); }
      list(const list& __x)
      : _Base(__x._M_get_Node_allocator())
      { _M_initialize_dispatch(__x.begin(), __x.end(), __false_type()); }
      template<typename _InputIterator>
	list(_InputIterator __first, _InputIterator __last,
      const allocator_type& __a = allocator_type())
	: _Base(__a)
	{
   typedef typename std::__is_integer<_InputIterator>::__type _Integral;
   _M_initialize_dispatch(__first, __last, _Integral());
 }
      list&
      operator=(const list& __x);
      void
      assign(size_type __n, const value_type& __val)
      { _M_fill_assign(__n, __val); }
      template<typename _InputIterator>
	void
	assign(_InputIterator __first, _InputIterator __last)
	{
   typedef typename std::__is_integer<_InputIterator>::__type _Integral;
   _M_assign_dispatch(__first, __last, _Integral());
 }
      allocator_type
      get_allocator() const
      { return _Base::get_allocator(); }
      iterator
      begin()
      { return iterator(this->_M_impl._M_node._M_next); }
      const_iterator
      begin() const
      { return const_iterator(this->_M_impl._M_node._M_next); }
      iterator
      end()
      { return iterator(&this->_M_impl._M_node); }
      const_iterator
      end() const
      { return const_iterator(&this->_M_impl._M_node); }
      reverse_iterator
      rbegin()
      { return reverse_iterator(end()); }
      const_reverse_iterator
      rbegin() const
      { return const_reverse_iterator(end()); }
      reverse_iterator
      rend()
      { return reverse_iterator(begin()); }
      const_reverse_iterator
      rend() const
      { return const_reverse_iterator(begin()); }
      bool
      empty() const
      { return this->_M_impl._M_node._M_next == &this->_M_impl._M_node; }
      size_type
      size() const
      { return std::distance(begin(), end()); }
      size_type
      max_size() const
      { return _M_get_Node_allocator().max_size(); }
      void
      resize(size_type __new_size, value_type __x = value_type());
      reference
      front()
      { return *begin(); }
      const_reference
      front() const
      { return *begin(); }
      reference
      back()
      {
 iterator __tmp = end();
 --__tmp;
 return *__tmp;
      }
      const_reference
      back() const
      {
 const_iterator __tmp = end();
 --__tmp;
 return *__tmp;
      }
      void
      push_front(const value_type& __x)
      { this->_M_insert(begin(), __x); }
      void
      pop_front()
      { this->_M_erase(begin()); }
      void
      push_back(const value_type& __x)
      { this->_M_insert(end(), __x); }
      void
      pop_back()
      { this->_M_erase(iterator(this->_M_impl._M_node._M_prev)); }
      iterator
      insert(iterator __position, const value_type& __x);
      void
      insert(iterator __position, size_type __n, const value_type& __x)
      {
 list __tmp(__n, __x, _M_get_Node_allocator());
 splice(__position, __tmp);
      }
      template<typename _InputIterator>
	void
	insert(iterator __position, _InputIterator __first,
	_InputIterator __last)
	{
   list __tmp(__first, __last, _M_get_Node_allocator());
   splice(__position, __tmp);
 }
      iterator
      erase(iterator __position);
      iterator
      erase(iterator __first, iterator __last)
      {
 while (__first != __last)
   __first = erase(__first);
 return __last;
      }
      void
      swap(list& __x)
      {
 _List_node_base::swap(this->_M_impl._M_node, __x._M_impl._M_node);
 std::__alloc_swap<typename _Base::_Node_alloc_type>::
   _S_do_it(_M_get_Node_allocator(), __x._M_get_Node_allocator());
      }
      void
      clear()
      {
	_Base::_M_clear();
	_Base::_M_init();
      }
      void
      splice(iterator __position, list& __x)
      {
 if (!__x.empty())
   {
     _M_check_equal_allocators(__x);
     this->_M_transfer(__position, __x.begin(), __x.end());
   }
      }
      void
      splice(iterator __position, list& __x, iterator __i)
      {
 iterator __j = __i;
 ++__j;
 if (__position == __i || __position == __j)
   return;
 if (this != &__x)
   _M_check_equal_allocators(__x);
 this->_M_transfer(__position, __i, __j);
      }
      void
      splice(iterator __position, list& __x, iterator __first,
      iterator __last)
      {
 if (__first != __last)
   {
     if (this != &__x)
       _M_check_equal_allocators(__x);
     this->_M_transfer(__position, __first, __last);
   }
      }
      void
      remove(const _Tp& __value);
      template<typename _Predicate>
	void
	remove_if(_Predicate);
      void
      unique();
      template<typename _BinaryPredicate>
	void
	unique(_BinaryPredicate);
      void
      merge(list& __x);
      template<typename _StrictWeakOrdering>
	void
	merge(list&, _StrictWeakOrdering);
      void
      reverse()
      { this->_M_impl._M_node.reverse(); }
      void
      sort();
      template<typename _StrictWeakOrdering>
	void
	sort(_StrictWeakOrdering);
    protected:
      template<typename _Integer>
	void
	_M_initialize_dispatch(_Integer __n, _Integer __x, __true_type)
	{ _M_fill_initialize(static_cast<size_type>(__n), __x); }
      template<typename _InputIterator>
	void
	_M_initialize_dispatch(_InputIterator __first, _InputIterator __last,
	  __false_type)
	{
   for (; __first != __last; ++__first)
     push_back(*__first);
 }
      void
      _M_fill_initialize(size_type __n, const value_type& __x)
      {
 for (; __n > 0; --__n)
   push_back(__x);
      }
      template<typename _Integer>
	void
	_M_assign_dispatch(_Integer __n, _Integer __val, __true_type)
	{ _M_fill_assign(__n, __val); }
      template<typename _InputIterator>
	void
	_M_assign_dispatch(_InputIterator __first, _InputIterator __last,
      __false_type);
      void
      _M_fill_assign(size_type __n, const value_type& __val);
      void
      _M_transfer(iterator __position, iterator __first, iterator __last)
      { __position._M_node->transfer(__first._M_node, __last._M_node); }
      void
      _M_insert(iterator __position, const value_type& __x)
      {
	_Node* __tmp = _M_create_node(__x);
	__tmp->hook(__position._M_node);
      }
      void
      _M_erase(iterator __position)
      {
	__position._M_node->unhook();
	_Node* __n = static_cast<_Node*>(__position._M_node);
 _M_get_Tp_allocator().destroy(&__n->_M_data);
	_M_put_node(__n);
      }
      void
      _M_check_equal_allocators(list& __x)
      {
 if (std::__alloc_neq<typename _Base::_Node_alloc_type>::
     _S_do_it(_M_get_Node_allocator(), __x._M_get_Node_allocator()))
   __throw_runtime_error(("list::_M_check_equal_allocators"));
      }
    };
  template<typename _Tp, typename _Alloc>
    inline bool
    operator==(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    {
      typedef typename list<_Tp, _Alloc>::const_iterator const_iterator;
      const_iterator __end1 = __x.end();
      const_iterator __end2 = __y.end();
      const_iterator __i1 = __x.begin();
      const_iterator __i2 = __y.begin();
      while (__i1 != __end1 && __i2 != __end2 && *__i1 == *__i2)
 {
   ++__i1;
   ++__i2;
 }
      return __i1 == __end1 && __i2 == __end2;
    }
  template<typename _Tp, typename _Alloc>
    inline bool
    operator<(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    { return std::lexicographical_compare(__x.begin(), __x.end(),
       __y.begin(), __y.end()); }
  template<typename _Tp, typename _Alloc>
    inline bool
    operator!=(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    { return !(__x == __y); }
  template<typename _Tp, typename _Alloc>
    inline bool
    operator>(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    { return __y < __x; }
  template<typename _Tp, typename _Alloc>
    inline bool
    operator<=(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    { return !(__y < __x); }
  template<typename _Tp, typename _Alloc>
    inline bool
    operator>=(const list<_Tp, _Alloc>& __x, const list<_Tp, _Alloc>& __y)
    { return !(__x < __y); }
  template<typename _Tp, typename _Alloc>
    inline void
    swap(list<_Tp, _Alloc>& __x, list<_Tp, _Alloc>& __y)
    { __x.swap(__y); }
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp, typename _Alloc>
    void
    _List_base<_Tp, _Alloc>::
    _M_clear()
    {
      typedef _List_node<_Tp> _Node;
      _Node* __cur = static_cast<_Node*>(this->_M_impl._M_node._M_next);
      while (__cur != &this->_M_impl._M_node)
 {
   _Node* __tmp = __cur;
   __cur = static_cast<_Node*>(__cur->_M_next);
   _M_get_Tp_allocator().destroy(&__tmp->_M_data);
   _M_put_node(__tmp);
 }
    }
  template<typename _Tp, typename _Alloc>
    typename list<_Tp, _Alloc>::iterator
    list<_Tp, _Alloc>::
    insert(iterator __position, const value_type& __x)
    {
      _Node* __tmp = _M_create_node(__x);
      __tmp->hook(__position._M_node);
      return iterator(__tmp);
    }
  template<typename _Tp, typename _Alloc>
    typename list<_Tp, _Alloc>::iterator
    list<_Tp, _Alloc>::
    erase(iterator __position)
    {
      iterator __ret = iterator(__position._M_node->_M_next);
      _M_erase(__position);
      return __ret;
    }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    resize(size_type __new_size, value_type __x)
    {
      iterator __i = begin();
      size_type __len = 0;
      for (; __i != end() && __len < __new_size; ++__i, ++__len)
	;
      if (__len == __new_size)
	erase(__i, end());
      else
	insert(end(), __new_size - __len, __x);
    }
  template<typename _Tp, typename _Alloc>
    list<_Tp, _Alloc>&
    list<_Tp, _Alloc>::
    operator=(const list& __x)
    {
      if (this != &__x)
 {
   iterator __first1 = begin();
   iterator __last1 = end();
   const_iterator __first2 = __x.begin();
   const_iterator __last2 = __x.end();
   for (; __first1 != __last1 && __first2 != __last2;
	++__first1, ++__first2)
     *__first1 = *__first2;
   if (__first2 == __last2)
     erase(__first1, __last1);
   else
     insert(__last1, __first2, __last2);
 }
      return *this;
    }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    _M_fill_assign(size_type __n, const value_type& __val)
    {
      iterator __i = begin();
      for (; __i != end() && __n > 0; ++__i, --__n)
	*__i = __val;
      if (__n > 0)
	insert(end(), __n, __val);
      else
	erase(__i, end());
    }
  template<typename _Tp, typename _Alloc>
    template <typename _InputIterator>
      void
      list<_Tp, _Alloc>::
      _M_assign_dispatch(_InputIterator __first2, _InputIterator __last2,
    __false_type)
      {
	iterator __first1 = begin();
	iterator __last1 = end();
	for (; __first1 != __last1 && __first2 != __last2;
      ++__first1, ++__first2)
	  *__first1 = *__first2;
	if (__first2 == __last2)
	  erase(__first1, __last1);
	else
	  insert(__last1, __first2, __last2);
      }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    remove(const value_type& __value)
    {
      iterator __first = begin();
      iterator __last = end();
      iterator __extra = __last;
      while (__first != __last)
 {
   iterator __next = __first;
   ++__next;
   if (*__first == __value)
     {
       if (&*__first != &__value)
  _M_erase(__first);
       else
  __extra = __first;
     }
   __first = __next;
 }
      if (__extra != __last)
 _M_erase(__extra);
    }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    unique()
    {
      iterator __first = begin();
      iterator __last = end();
      if (__first == __last)
 return;
      iterator __next = __first;
      while (++__next != __last)
 {
   if (*__first == *__next)
     _M_erase(__next);
   else
     __first = __next;
   __next = __first;
 }
    }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    merge(list& __x)
    {
      if (this != &__x)
 {
   _M_check_equal_allocators(__x);
   iterator __first1 = begin();
   iterator __last1 = end();
   iterator __first2 = __x.begin();
   iterator __last2 = __x.end();
   while (__first1 != __last1 && __first2 != __last2)
     if (*__first2 < *__first1)
       {
  iterator __next = __first2;
  _M_transfer(__first1, __first2, ++__next);
  __first2 = __next;
       }
     else
       ++__first1;
   if (__first2 != __last2)
     _M_transfer(__last1, __first2, __last2);
 }
    }
  template<typename _Tp, typename _Alloc>
    template <typename _StrictWeakOrdering>
      void
      list<_Tp, _Alloc>::
      merge(list& __x, _StrictWeakOrdering __comp)
      {
 if (this != &__x)
   {
     _M_check_equal_allocators(__x);
     iterator __first1 = begin();
     iterator __last1 = end();
     iterator __first2 = __x.begin();
     iterator __last2 = __x.end();
     while (__first1 != __last1 && __first2 != __last2)
       if (__comp(*__first2, *__first1))
  {
    iterator __next = __first2;
    _M_transfer(__first1, __first2, ++__next);
    __first2 = __next;
  }
       else
  ++__first1;
     if (__first2 != __last2)
       _M_transfer(__last1, __first2, __last2);
   }
      }
  template<typename _Tp, typename _Alloc>
    void
    list<_Tp, _Alloc>::
    sort()
    {
      if (this->_M_impl._M_node._M_next != &this->_M_impl._M_node
   && this->_M_impl._M_node._M_next->_M_next != &this->_M_impl._M_node)
      {
	list __carry;
	list __tmp[64];
	list * __fill = &__tmp[0];
	list * __counter;
	do
   {
     __carry.splice(__carry.begin(), *this, begin());
     for(__counter = &__tmp[0];
  __counter != __fill && !__counter->empty();
  ++__counter)
       {
  __counter->merge(__carry);
  __carry.swap(*__counter);
       }
     __carry.swap(*__counter);
     if (__counter == __fill)
       ++__fill;
   }
 while ( !empty() );
	for (__counter = &__tmp[1]; __counter != __fill; ++__counter)
	  __counter->merge(*(__counter - 1));
	swap( *(__fill - 1) );
      }
    }
  template<typename _Tp, typename _Alloc>
    template <typename _Predicate>
      void
      list<_Tp, _Alloc>::
      remove_if(_Predicate __pred)
      {
	iterator __first = begin();
	iterator __last = end();
	while (__first != __last)
   {
     iterator __next = __first;
     ++__next;
     if (__pred(*__first))
       _M_erase(__first);
     __first = __next;
   }
      }
  template<typename _Tp, typename _Alloc>
    template <typename _BinaryPredicate>
      void
      list<_Tp, _Alloc>::
      unique(_BinaryPredicate __binary_pred)
      {
	iterator __first = begin();
	iterator __last = end();
	if (__first == __last)
   return;
	iterator __next = __first;
	while (++__next != __last)
   {
     if (__binary_pred(*__first, *__next))
       _M_erase(__next);
     else
       __first = __next;
     __next = __first;
   }
      }
  template<typename _Tp, typename _Alloc>
    template <typename _StrictWeakOrdering>
      void
      list<_Tp, _Alloc>::
      sort(_StrictWeakOrdering __comp)
      {
 if (this->_M_impl._M_node._M_next != &this->_M_impl._M_node
     && this->_M_impl._M_node._M_next->_M_next != &this->_M_impl._M_node)
   {
     list __carry;
     list __tmp[64];
     list * __fill = &__tmp[0];
     list * __counter;
     do
       {
  __carry.splice(__carry.begin(), *this, begin());
  for(__counter = &__tmp[0];
      __counter != __fill && !__counter->empty();
      ++__counter)
    {
      __counter->merge(__carry, __comp);
      __carry.swap(*__counter);
    }
  __carry.swap(*__counter);
  if (__counter == __fill)
    ++__fill;
       }
     while ( !empty() );
     for (__counter = &__tmp[1]; __counter != __fill; ++__counter)
       __counter->merge(*(__counter - 1), __comp);
     swap(*(__fill - 1));
   }
      }
}
extern void foobarit(void);
class Game
{
public:
	struct BuildProject
	{
		int posX;
	};
	std::list<BuildProject> buildProjects;
};
static Game game;
static std::list<std::list<Game::BuildProject>::iterator>
erasableBuildProjects;
void *buildProjectSyncStepConcurrently(int id, int localTeam)
{
	__transaction_relaxed {
		std::list<std::list<Game::BuildProject>::iterator>::iterator it
= erasableBuildProjects.begin();
		foobarit();
		game.buildProjects.erase( (std::list<Game::BuildProject>
::iterator) *it);
	}
	return 0;
}

