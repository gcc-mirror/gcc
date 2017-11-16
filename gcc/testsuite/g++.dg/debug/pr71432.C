/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

namespace std
{
  typedef long unsigned int size_t;
  inline namespace __cxx11
  {
  } }

extern "C++"
{
  namespace std
  {
    template < typename _Tp > struct __is_char
    {
    };
    template <> struct __is_char <char >
    {
      enum
	{ __value = 1 };
    };
  } namespace __gnu_cxx
  {
    template < bool, typename > struct __enable_if
    {
    };
    template < typename _Tp > struct __enable_if <true, _Tp >
    {
      typedef _Tp __type;
    };
  }
}

namespace __gnu_cxx
{
  template < typename _Tp > class new_allocator
  {
  };
}

namespace std
{
  template < typename _Tp > using __allocator_base =
    __gnu_cxx::new_allocator < _Tp >;
template < typename _Tp > class allocator:public __allocator_base < _Tp >
  {
  };
  template < typename _Alloc > struct allocator_traits
  {
  };
  template < typename _Tp > struct allocator_traits <allocator < _Tp >>
  {
    using size_type = std::size_t;
    template < typename _Up > using rebind_alloc = allocator < _Up >;
  };
}

namespace __gnu_cxx
{
  template < typename _Alloc > struct __alloc_traits:std::allocator_traits <_Alloc >
  {
    typedef std::allocator_traits < _Alloc > _Base_type;
     template < typename _Tp > struct rebind
    {
      typedef typename _Base_type::template rebind_alloc < _Tp > other;
     };
   };
}

namespace std
{
  template < class _CharT > struct char_traits;
  namespace __cxx11
  {
    template < typename _CharT, typename _Traits =
      char_traits < _CharT >, typename _Alloc =
      allocator < _CharT > >class basic_string;
    typedef basic_string < char >string;
  }
}

namespace std
{
  template <> struct char_traits <char >
  {
    typedef char char_type;
     static int compare (const char_type * __s1, const char_type * __s2,
			  size_t __n)
    {
      return __builtin_memcmp (__s1, __s2, __n);
    }
  };

  namespace __cxx11
  {
    template < typename _CharT, typename _Traits, typename _Alloc >
    class basic_string
    {
      typedef typename __gnu_cxx::__alloc_traits <_Alloc >::template rebind < _CharT >::other _Char_alloc_type;
      typedef __gnu_cxx::__alloc_traits < _Char_alloc_type > _Alloc_traits;
      typedef typename _Alloc_traits::size_type size_type;

    public:
      size_type size ()const noexcept
      {
	return 0;
      }
      const _CharT *data () const noexcept
      {
	return 0;
      }
    };
  }

  template < typename _CharT > inline typename __gnu_cxx::__enable_if <
    __is_char < _CharT >::__value,
    bool >::__type operator== (const basic_string < _CharT > &__lhs,
			       const basic_string < _CharT > &__rhs) noexcept
  {
    return !std::char_traits < _CharT >::compare (__lhs.data (),
						   __rhs.data (),
						   __lhs.size ());
  }
};

class CLIParameterType
{
  const std::string & getSwitchOption (unsigned int i) const
  {
    static std::string a;
    return a;
  }
  unsigned int getSwitchOptionCount () const
  {
    return 0;
  }
  int checkSwitched (const std::string & value) const;
};

int
CLIParameterType::checkSwitched (const std::string & value) const
{
  int contains = false;
  for (unsigned int i = 0; !contains && i < getSwitchOptionCount () ;)
    contains = getSwitchOption (i) == value;

  return 0;
}
