// { dg-lto-do link }
// { dg-lto-options {{-flto -O0 -std=c++11}} }

/* pr65276_0.C should get compiled with -O0, while the _1.C file
   should get compiled with -O2, and the entire thing should be linked
   with -O0.  Test that we don't get an ICE.  */

extern "C++"
{
  namespace std2
  {
    class exception
    {
    public:
      virtual ~ exception () noexcept;
    };
  }
}
namespace std2
{
  struct __cow_string
  {
    union
    {
      const char *_M_p;
      char _M_bytes[sizeof (const char *)];
    };
  };
  class runtime_error:public exception
  {
    __cow_string _M_msg;
  };
}
namespace std2
{
  class system_error:public std2::runtime_error
  {
  };
  enum _Ios_Fmtflags
  {
  };
  inline constexpr _Ios_Fmtflags operator& (_Ios_Fmtflags __a,
                                            _Ios_Fmtflags __b)
  {
    return _Ios_Fmtflags ();
  }
  enum _Ios_Openmode
  {
  };
  class ios_base
  {
  public:
    class __attribute ((__abi_tag__ ("cxx11"))) failure:public system_error
    {
    };
    class Init
    {
    };
  };
  static ios_base::Init __ioinit;
}

std2::exception::~exception() noexcept { }
