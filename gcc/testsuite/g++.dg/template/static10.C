// PR c++/19555

namespace __gnu_debug_def { }
namespace std
{
  using namespace __gnu_debug_def;
  template<typename _Tp> class allocator {};
}
namespace __gnu_debug_def
{
  template<typename _Tp,
    typename _Allocator = std::allocator<_Tp> >
    class vector
    {
      void
      swap(vector<_Tp,_Allocator>& __x);
    };
}
namespace std
{
  template<> void
  vector<int, allocator<int> >::swap(vector<int, allocator<int> >&) { } // { dg-error "" }
}
