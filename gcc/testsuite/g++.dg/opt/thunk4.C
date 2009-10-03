// { dg-do compile }
// { dg-options "-O1" }
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < class _CharT > struct char_traits;
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  template < typename _CharT, typename _Traits =
    char_traits < _CharT > >class basic_iostream;
}

extern "C++"
{
  namespace std
  {
    class exception
    {
    public:exception () throw ()
      {
      }
    };
  }
}
namespace std __attribute__ ((__visibility__ ("default")))
{
  class ios_base
  {
  public:class failure:public exception
    {
    };
    virtual ~ ios_base ();
  };
template < typename _CharT, typename _Traits > class basic_ios:public
    ios_base
  {
  };
template < typename _CharT, typename _Traits > class basic_ostream:virtual public basic_ios < _CharT,
    _Traits
    >
  {
  };
}
namespace std __attribute__ ((__visibility__ ("default")))
{
template < typename _CharT, typename _Traits > class basic_istream:virtual public basic_ios < _CharT,
    _Traits
    >
  {
  };
template < typename _CharT, typename _Traits > class basic_iostream:public basic_istream < _CharT, _Traits >, public basic_ostream < _CharT,
    _Traits
    >
  {
  };
  class strstream:public basic_iostream < char >
  {
    virtual ~ strstream ();
  };
  strstream::~strstream ()
  {
  }
}
