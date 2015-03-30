// PR sanitizer/65583
// { dg-do compile }
// { dg-options "-std=c++11 -fsanitize=undefined" }

namespace std
{
  inline namespace __cxx11
  {
  }
  template < typename > class allocator;
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
  template < typename _Tp, _Tp __v > struct integral_constant
  {
    static constexpr _Tp value = __v;
  };
  typedef integral_constant < bool, true > true_type;
}
namespace __gnu_cxx
{
  template < typename _Tp > class new_allocator
  {
  public:
    typedef long unsigned size_type;
    typedef _Tp value_type;
      template < typename _Tp1 > struct rebind
    {
      typedef new_allocator < _Tp1 > other;
    };
  };
}
namespace std
{
  template < typename _Tp > using __allocator_base =
    __gnu_cxx::new_allocator < _Tp >;
  template < typename _Tp > class allocator:public __allocator_base < _Tp >
  {
  };
  template < typename _Alloc, typename _Tp > class __alloctr_rebind_helper
  {
    template < typename _Alloc2, typename _Tp2 >
      static constexpr true_type _S_chk (typename _Alloc2::template rebind <
					 _Tp2 >::other *);
  public:
    using __type = decltype (_S_chk < _Alloc, _Tp > (nullptr));
  };
  template < typename _Alloc, typename _Tp, bool =
    __alloctr_rebind_helper < _Alloc,
    _Tp >::__type::value > struct __alloctr_rebind;
  template < typename _Alloc, typename _Tp > struct __alloctr_rebind <_Alloc,
    _Tp, true >
  {
    typedef typename _Alloc::template rebind < _Tp >::other __type;
  };
  template < typename _Alloc > struct allocator_traits
  {
    typedef typename _Alloc::value_type value_type;
    static value_type *_S_pointer_helper (...);
    typedef decltype (_S_pointer_helper ((_Alloc *) 0)) __pointer;
    typedef __pointer pointer;
      template < typename _Tp >
      static typename _Tp::size_type _S_size_type_helper (_Tp *);
    typedef decltype (_S_size_type_helper ((_Alloc *) 0)) __size_type;
    typedef __size_type size_type;
      template < typename _Tp > using rebind_alloc =
      typename __alloctr_rebind < _Alloc, _Tp >::__type;
  };
}
namespace __gnu_cxx
{
  template < typename _Alloc > struct __alloc_traits:std::allocator_traits <
    _Alloc >
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
  namespace __cxx11
  {
    template < typename _CharT, typename _Traits,
      typename _Alloc > class basic_string
    {
      typedef typename __gnu_cxx::__alloc_traits < _Alloc >::template rebind <
	_CharT >::other _Char_alloc_type;
      typedef __gnu_cxx::__alloc_traits < _Char_alloc_type > _Alloc_traits;
      typedef _Char_alloc_type allocator_type;
      typedef typename _Alloc_traits::size_type size_type;
      typedef typename _Alloc_traits::pointer pointer;
      struct _Alloc_hider:allocator_type
      {
	_Alloc_hider (pointer __dat, const _Alloc & __a)
	{
	}
      };
      _Alloc_hider _M_dataplus;
      union
      {
	size_type _M_allocated_capacity;
      };
      pointer _M_local_data ()
      {
      }
      void _M_dispose ()
      {
	_M_destroy (_M_allocated_capacity);
      }
      void _M_destroy (size_type __size) throw ()
      {
      }
    public:
    basic_string (const _CharT * __s, const _Alloc & __a = _Alloc ()):_M_dataplus (_M_local_data (),
		   __a)
      {
	_M_dispose ();
      }
    };
  }
  class FileHandle
  {
    std::string fname;
    FileHandle (const char *fname);
  };
  FileHandle::FileHandle (const char *fname):fname (fname)
  {
  }
}
