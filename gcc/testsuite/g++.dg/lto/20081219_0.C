// { dg-lto-do link }
// { dg-lto-options {{-fPIC -flto -flto-partition=1to1 -O2}} }
// { dg-extra-ld-options "-O2 -fPIC -flto -flto-partition=1to1 -r -nostdlib" }

typedef long int ptrdiff_t;
extern "C"
{
  typedef struct
  {
  }
  __mbstate_t;
  namespace std
  {
    class exception
    {
    };
  }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _Alloc > class allocator;
  template < class _CharT > struct char_traits;
}
typedef __mbstate_t mbstate_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  using::mbstate_t;
  typedef ptrdiff_t streamsize;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class basic_istream;
  template < typename _CharT, typename _Traits =
    char_traits < _CharT >, typename _Alloc =
    allocator < _CharT > >class basic_stringbuf;
  class ios_base
  {
  public:class failure:public exception
    {
    };
    virtual ~ ios_base ();
  };
  template < typename _CharT, typename _Traits > class basic_streambuf
  {
  };
template < typename _CharT, typename _Traits > class basic_ios:public
    ios_base
  {
  };
template < typename _CharT, typename _Traits > class basic_istream:virtual public basic_ios < _CharT,
    _Traits
    >
  {
    typedef basic_streambuf < _CharT, _Traits > __streambuf_type;
  protected:streamsize _M_gcount;
  public: explicit basic_istream (__streambuf_type * __sb):_M_gcount (streamsize
	       (0))
    {
    }
  };
template < typename _CharT, typename _Traits, typename _Alloc > class basic_stringbuf:public basic_streambuf < _CharT,
    _Traits
    >
  {
  };
  template < typename V, typename I, typename S = std::mbstate_t > struct character
  {
  };
  typedef character < unsigned short, unsigned int >pod_ushort;
  typedef basic_stringbuf < pod_ushort > stringbuf_type;
  typedef basic_istream < pod_ushort > istream_type;
  stringbuf_type strbuf01;
  istream_type stream (&strbuf01);
}
