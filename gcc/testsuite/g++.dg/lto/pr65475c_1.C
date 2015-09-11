namespace std
{
  template < class _CharT > struct char_traits;
  typedef long streamsize;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class basic_ostream;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class Trans_NS___cxx11_basic_ostringstream;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class istreambuf_iterator;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class ostreambuf_iterator;
}
namespace std
{
  template < typename _CharT, typename _InIter =
    istreambuf_iterator < _CharT > >class num_get;
  template < typename _CharT, typename _OutIter =
    ostreambuf_iterator < _CharT > >class num_put;
  struct iterator
  {
  };
}
typedef int _Atomic_word;
namespace std
{
  class locale
  {
  public:class facet;
    class _Impl;
      template < typename _Facet > locale ( const locale & __other,
					    _Facet * __f );
  private:  _Impl * _M_impl;
  };
  class locale::facet
  {
    mutable _Atomic_word _M_refcount;
  protected:  explicit facet ( void ) throw (  );
      virtual ~ facet (  );
  };
  enum _Ios_Fmtflags
  {
    _S_boolalpha = 1 << 0, _S_dec, _S_fixed = 1 << 2, _S_hex =
      1 << 3, _S_internal = 1 << 4, _S_left = 1 << 5, _S_oct =
      1 << 6, _S_right = 1 << 7, _S_scientific = 1 << 8, _S_showbase =
      1 << 9, _S_showpoint = 1 << 10, _S_showpos = 1 << 11, _S_skipws =
      1 << 12, _S_unitbuf = 1 << 13, _S_uppercase = 1 << 14, _S_adjustfield =
      _S_left | _S_right | _S_internal, _S_basefield =
      _S_dec | _S_oct | _S_hex, _S_floatfield =
      _S_scientific | _S_fixed, _S_ios_fmtflags_end = 1 << 16
  };
  enum _Ios_Openmode
  {
    _S_out
  };
  enum _Ios_Iostate
  {
    _S_goodbit, _S_badbit, _S_eofbit, _S_failbit =
      1 << 2, _S_ios_iostate_end = 1 << 16
  };
  class ios_base
  {
  public:typedef _Ios_Fmtflags fmtflags;
    typedef _Ios_Iostate iostate;
  protected:  streamsize _M_precision;
    streamsize _M_width;
    fmtflags _M_flags;
    iostate _M_exception;
    iostate _M_streambuf_state;
    struct _Callback_list;
    _Callback_list *_M_callbacks;
    struct _Words
    {
      void *_M_pword;
      long _M_iword;
    };
    _Words _M_word_zero;
    enum
    {
      _S_local_word_size = 8
    };
    _Words _M_local_word[_S_local_word_size];
    int _M_word_size;
    _Words *_M_word;
    locale _M_ios_locale;
      virtual ~ ios_base (  );
  };
  template < typename _CharT, typename _Traits > class basic_streambuf;
template < typename _CharT, typename _Traits > class ostreambuf_iterator:public
    iterator
  {
    typedef basic_ostream < wchar_t, _Traits > ostream_type;
  };
  class __ctype_abstract_base:public locale::facet
  {
  };
template < typename _CharT > class ctype:public __ctype_abstract_base
  {
  };
  class Trans_NS___cxx11_numpunct:public locale::facet
  {
  };
template < typename _CharT, typename _InIter > class num_get:public locale::
    facet
  {
  };
template < typename _CharT, typename _OutIter > class num_put:public locale::
    facet
  {
  public:typedef int char_type;
    typedef std::ostreambuf_iterator < wchar_t,
      std::char_traits < wchar_t > >iter_type;
  };
template < typename _CharT, typename _Traits > class basic_ios:public
    ios_base
  {
  public:typedef wchar_t char_type;
    typedef num_get < wchar_t, istreambuf_iterator < wchar_t,
      _Traits > >__num_get_type;
  protected:basic_ostream < wchar_t, _Traits > *_M_tie;
    mutable char_type _M_fill;
    mutable bool _M_fill_init;
    basic_streambuf < wchar_t, _Traits > *_M_streambuf;
    const ctype < wchar_t > *_M_ctype;
    const num_put < wchar_t, ostreambuf_iterator < wchar_t,
      _Traits > >*_M_num_put;
    const __num_get_type *_M_num_get;
  };
template < typename _CharT, typename _Traits > class basic_ostream:virtual public basic_ios < wchar_t,
    _Traits
    >
  {
  };
}
typedef enum
{
  posix
}
value_type;
static const unsigned int wchar_t_facet = 1 << 1;
class shared_ptr
{
};
namespace std 
{
template < typename _CharT, typename _Traits > class Trans_NS___cxx11_basic_ostringstream:public basic_ostream < wchar_t,
    _Traits
    >
  {
  public:explicit Trans_NS___cxx11_basic_ostringstream ( void );
  };
}
class base_num_format:public
  std::num_put <
  wchar_t >
{
public:typedef typename
    std::num_put <
    wchar_t >::iter_type
    iter_type;
  typedef wchar_t
    char_type;
  base_num_format ( unsigned long refs = 0 );
  iter_type
    do_put_out;
  std::ios_base &
    do_put_ios;
  char_type
    do_put_fill;
  unsigned long long
    do_put_val;
  virtual iter_type
  do_put ( void ) const
  {
    return
    do_real_put ( do_put_out, do_put_ios, do_put_fill, do_put_val );
  }
private:template <
    typename
    ValueType >
    iter_type
  do_real_put ( iter_type out, std::ios_base & ios, char_type fill,
		ValueType val ) const
  {
    switch ( 0 )
      case posix:
      {
	typedef
	  std::Trans_NS___cxx11_basic_ostringstream <
	  char_type >
	  sstream_type;
	sstream_type
	  ss;
      }
  }
};
class
  base_num_parse:
  public
  std::num_get <
  wchar_t >
{
private:};
class
  num_format:
  public
  base_num_format
{
public:typedef wchar_t
    iter_type;
  num_format ( shared_ptr lc, unsigned long refs = 0 )
  {
  }
};
class
  num_punct_posix:
  public
  std::Trans_NS___cxx11_numpunct
{
};
template < typename CharType >
  std::locale create_formatting_impl ( std::locale const &in, shared_ptr lc )
{
  std::locale tmp = std::locale ( tmp, new num_format ( lc ) );
}
shared_ptr create_formatting_lc;
unsigned int create_formatting_type;
void
create_formatting ( std::locale const &in )
{
  switch ( create_formatting_type )
    case wchar_t_facet:
    create_formatting_impl < wchar_t > ( in, create_formatting_lc );
}
