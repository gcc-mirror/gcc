// { dg-do compile }
// { dg-options "-O3 -ftracer -fsched-pressure -Wno-unused-parameter -Wno-return-type" }

extern "C"
{
  namespace std
  {
    class exception
    {
      virtual const char *what () const throw ();
    };
  }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Alloc > class allocator;
  template < class _CharT > struct char_traits;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT >, typename _Alloc =
    allocator < _CharT > >class basic_string;
  typedef basic_string < char >string;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class basic_ios;
  typedef basic_ios < char >ios;
}

namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  template < typename _Tp > class new_allocator
  {
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
template < typename _Tp > class allocator:public __gnu_cxx::new_allocator <
    _Tp >
  {
  };
}

typedef int _Atomic_word;
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  static inline _Atomic_word
    __attribute__ ((__unused__)) __exchange_and_add_dispatch (_Atomic_word *
							      __mem,
							      int __val)
  {
  }
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _CharT, typename _Traits,
    typename _Alloc > class basic_string
  {
    typedef _Alloc allocator_type;
  private:struct _Rep_base
    {
      _Atomic_word _M_refcount;
    };
    struct _Rep:_Rep_base
    {
      void _M_dispose (const _Alloc & __a)
      {
	if (__builtin_expect (this != &_S_empty_rep (), false))
	  {
	    if (__gnu_cxx::
		__exchange_and_add_dispatch (&this->_M_refcount, -1) <= 0)
	      {
		_M_destroy (__a);
	      }
	  }
      }
      void _M_destroy (const _Alloc &) throw ();
    };
    struct _Alloc_hider:_Alloc
    {
      _CharT *_M_p;
    };
  private:mutable _Alloc_hider _M_dataplus;
    _CharT *_M_data () const
    {
      return _M_dataplus._M_p;
    }
    _Rep *_M_rep () const
    {
      return &((reinterpret_cast < _Rep * >(_M_data ()))[-1]);
    }
    static _Rep & _S_empty_rep ()
    {
    }
  public: basic_string ():_M_dataplus (_S_empty_rep ()._M_refdata (),
		 _Alloc ())
    {
    }
    basic_string (const _CharT * __s, const _Alloc & __a = _Alloc ());
    ~basic_string ()
    {
      _M_rep ()->_M_dispose (this->get_allocator ());
    }
    allocator_type get_allocator () const
    {
    }
  };
  class ios_base
  {
  public:class failure:public exception
    {
    public:explicit failure (const string & __str) throw ();
    };
  };
template < typename _CharT, typename _Traits > class basic_ios:public
    ios_base
  {
  };
  namespace iostreams
  {
    class zlib_error:public std::ios::failure
    {
    public:explicit zlib_error (int error);
    private:int error_;
    };
      zlib_error::zlib_error (int error):std::ios::failure ("zlib error"),
      error_ (error)
    {
    }
  }
}
