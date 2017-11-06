// { dg-lto-do link }
// { dg-lto-options { { -flto -std=c++11 -g2 -fno-lto-odr-type-merging -O2 -Wno-return-type } } }
// { dg-extra-ld-options "-r -nostdlib -O2 -fno-lto-odr-type-merging" }

namespace std
{
  typedef long unsigned int size_t;
}
extern "C"
{
  typedef struct
  {
  } __mbstate_t;
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < class _CharT > struct char_traits;
}

typedef __mbstate_t mbstate_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class basic_ostream;
  typedef basic_ostream < char >ostream;
}

using namespace std;
class Cstring
{
public:
  Cstring (const char *str, int l = 0);
};
extern ostream & operator << (ostream & os, const Cstring & string);
class Foo_Log_Handler
{
  virtual int write_message (const char *msg, size_t msg_len, int channel,
			     int level) = 0;
};
class Foo_Log_Handler_Stream:public Foo_Log_Handler
{
  virtual int write_message (const char *msg, size_t msg_len, int channel,
			     int level) override;
  Cstring m_filename;
};
namespace std __attribute__ ((__visibility__ ("default")))
{
  template <> struct char_traits <char >
  {
    typedef mbstate_t state_type;
  };
  enum _Ios_Fmtflags
  {
  };
  enum _Ios_Iostate
  {
  };
  class ios_base
  {
  public:
    typedef _Ios_Iostate iostate;
  };
}

namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _CharT > class __ctype_abstract_base
  {
  };
  template < typename _CharT > class ctype
  {
  public:
    typedef char char_type;
    mutable char _M_widen_ok;
    char_type widen (char __c) const
    {
      if (_M_widen_ok)
	return this->do_widen (__c);
    }
    virtual char_type do_widen (char __c) const
    {
    }
  };
  template < typename _Facet >
    inline const _Facet & __check_facet (const _Facet * __f)
  {
  }
template < typename _CharT, typename _Traits > class basic_ios:public
    ios_base
  {
    typedef _CharT char_type;
    typedef ctype < _CharT > __ctype_type;
    const __ctype_type *_M_ctype;
  public:
    iostate rdstate ()const
    {
    }
    bool good () const
    {
    }
    char_type widen (char __c) const
    {
      return __check_facet (_M_ctype).widen (__c);
    }
  };
template < typename _CharT, typename _Traits > class basic_ostream:virtual public basic_ios < _CharT,
    _Traits
    >
  {
  public:
    typedef _CharT char_type;
    typedef _Traits traits_type;
    typedef basic_ostream < _CharT, _Traits > __ostream_type;
    __ostream_type & operator<< (__ostream_type & (*__pf) (__ostream_type &))
    {
      return __pf (*this);
    }
    __ostream_type & put (char_type __c);
  };
  template < typename _CharT,
    typename _Traits > inline basic_ostream < _CharT,
    _Traits > &endl (basic_ostream < _CharT, _Traits > &__os)
  {
    return flush (__os.put (__os.widen ('\n')));
  }
  template < typename _CharT,
    typename _Traits > inline basic_ostream < _CharT,
    _Traits > &flush (basic_ostream < _CharT, _Traits > &__os)
  {
  }
  extern ostream cerr;
}

int
Foo_Log_Handler_Stream::write_message (const char *msg, size_t msg_len, int,
					 int level)
{
  {
    {
      cerr << "FATAL: cannot write into log file: " << m_filename << endl;
    }
  }
}
