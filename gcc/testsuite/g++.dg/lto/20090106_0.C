// { dg-lto-do link }
typedef long unsigned int size_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
  using ::size_t;
  template<typename _Tp>
    struct __is_char
    {
    };
# 422 "/usr/include/c++/4.4.0/bits/cpp_type_traits.h" 3
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<bool, typename>
    struct __enable_if
    {
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<class _T1, class _T2>
    struct pair
    {
    };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    class new_allocator
    {
    };
}
namespace std __attribute__ ((__visibility__ ("default"))) {
  template<typename _Tp>
    class allocator: public __gnu_cxx::new_allocator<_Tp>
    {
  };
  template<typename _Arg1, typename _Arg2, typename _Result>
    struct binary_function
    {
    };
  template<typename _Tp>
    struct less : public binary_function<_Tp, _Tp, bool>
    {
    };
  template<typename _CharT>
    struct char_traits
    {
      typedef _CharT char_type;
      static std::size_t
      length(const char_type* __s);
    };
  template<typename _CharT>
    std::size_t
    char_traits<_CharT>::
    length(const char_type* __p)
    {
  }
  template<typename _CharT, typename _Traits = char_traits<_CharT> >
    class istreambuf_iterator;
  template<typename _CharT, typename _Traits, typename _Alloc>
    class basic_string
    {
  };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default"))) {
  template<typename _CharT, typename _Traits, typename _Alloc>
    class __versa_string;
  template<typename _CharT, typename _Traits, typename _Alloc>
    struct __vstring_utility
    {
    };
 template<typename _CharT, typename _Traits, typename _Alloc>
    class __rc_string_base
    {
      typedef __vstring_utility<_CharT, _Traits, _Alloc> _Util_Base;
      typedef typename _Util_Base::_CharT_alloc_type _CharT_alloc_type;
      typedef typename _CharT_alloc_type::size_type size_type;
      struct _Rep
      {
 union
 {
 };
 static _Rep*
 _S_create(size_type, size_type, const _Alloc&);
      };
    };
  template<typename _CharT, typename _Traits, typename _Alloc>
    typename __rc_string_base<_CharT, _Traits, _Alloc>::_Rep*
    __rc_string_base<_CharT, _Traits, _Alloc>::_Rep::
    _S_create(size_type __capacity, size_type __old_capacity,
       const _Alloc& __alloc)
    {
    };
}
template<typename _CharT, typename _Traits = std::char_traits<_CharT>,
         typename _Alloc = std::allocator<_CharT> >
class basic_string
    : public __gnu_cxx::__versa_string<_CharT, _Traits, _Alloc> {
};
template<typename _CharT, typename _Traits, typename _Alloc>
  operator+(const basic_string<_CharT, _Traits, _Alloc>& __lhs,
            const std::basic_string<_CharT, _Traits, _Alloc>& __rhs)
  {
}
namespace std __attribute__ ((__visibility__ ("default"))) {
    struct __uninitialized_copy
    {
      template<typename _InputIterator, typename _ForwardIterator>
        uninitialized_copy(_InputIterator __first, _InputIterator __last,
      _ForwardIterator __result)
        {
 }
    };
  template<typename _InputIterator, typename _ForwardIterator>
    uninitialized_copy(_InputIterator __first, _InputIterator __last,
         _ForwardIterator __result)
    {
    }
  class locale
  {
    class facet;
  };
  class locale::facet
    {
    };
  class ios_base
  {
      template<typename _CharT2>
 friend typename __gnu_cxx::__enable_if<__is_char<_CharT2>::__value,
               istreambuf_iterator<_CharT2> >::__type
 find(istreambuf_iterator<_CharT2>, istreambuf_iterator<_CharT2>,
      const _CharT2&);
    };
  template<typename _CharT, typename _OutIter>
    class num_put : public locale::facet
    {
      typedef _CharT char_type;
      typedef _OutIter iter_type;
      template<typename _ValueT>
        iter_type
        _M_insert_float(iter_type, ios_base& __io, char_type __fill,
   char __mod, _ValueT __v) const;
    };
  template<typename _CharT, typename _OutIter>
    template<typename _ValueT>
      _OutIter
      num_put<_CharT, _OutIter>::
      _M_insert_float(_OutIter __s, ios_base& __io, _CharT __fill, char __mod,
         _ValueT __v) const
      {
      }
  template<typename _CharT, typename _OutIter>
    class basic_ios : public ios_base
    {
    };
  template<typename _CharT, typename _Traits>
    class basic_istream : virtual public basic_ios<_CharT, _Traits>
    {
      typedef basic_istream<_CharT, _Traits> __istream_type;
      template<typename _ValueT>
        __istream_type&
        _M_extract(_ValueT& __v);
    };
  template<typename _CharT, typename _Traits>
    template<typename _ValueT>
      basic_istream<_CharT, _Traits>&
      basic_istream<_CharT, _Traits>::
      _M_extract(_ValueT& __v)
      {
      }
    class hash_map
    {
    };
}
class CDE {
 public:
  virtual ~CDE() { }
};
namespace std __attribute__ ((__visibility__ ("default"))) {
  template <typename _Key, typename _Tp, typename _Compare = std::less<_Key>,
            typename _Alloc = std::allocator<std::pair<const _Key, _Tp> > >
    class map
    {
    };
  template<typename _Key, typename _Tp, typename _Compare, typename _Alloc>
    operator==(const map<_Key, _Tp, _Compare, _Alloc>& __x,
              const map<_Key, _Tp, _Compare, _Alloc>& __y)
    { return !(__x < __y); }
}
namespace xyz {
class XYZ;
};
class ABC {
 public:
  virtual ~ABC() { }
};
class FGH : public CDE, public ABC {
 public:
  explicit FGH(CDE* efg);
};
namespace {
class LMN : public FGH {
  LMN(CDE* efg, xyz::XYZ* hij) : FGH(efg) { }
};
}
main(){}
