typedef struct
{
}
__mbstate_t;
typedef __mbstate_t mbstate_t;
namespace std __attribute__ ((__visibility__ ("default")))
{
  using::mbstate_t;
  typedef int *__c_locale;
  struct locale
  {
    class facet;
  };
  class locale::facet
  {
  };
template < typename _CharT > class numpunct:public locale::facet
  {
    void _M_initialize_numpunct (__c_locale __cloc = __null);
  };
}
namespace __gnu_cxx __attribute__ ((__visibility__ ("default")))
{
  template < typename V, typename I, typename S = std::mbstate_t > struct character
  {
  };
}

namespace __gnu_test
{
  using __gnu_cxx::character;
  typedef character < unsigned short, unsigned int >pod_ushort;
}
namespace std
{
  using __gnu_test::pod_ushort;
    template <> void numpunct <
    pod_ushort >::_M_initialize_numpunct (__c_locale)
  {
    pod_ushort *__truename = new pod_ushort[4 + 1];
  }
}
