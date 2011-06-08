// PR c++/49107
// { dg-options -std=c++0x }

template<typename _Tp>
_Tp declval() noexcept;

template<typename _Tp , typename = decltype(_Tp(declval<_Tp&&>()))>
struct trait
{
  static const bool value=true;
};

template<class _T2>
struct pair
{
  _T2 second;
  void swap(pair& __p)
    noexcept(trait<_T2>::value);
};

template < class R_ >
struct Main
{
  Main() {}
  Main(const typename R_::Sub1T& r) ;
  Main(const typename R_::Sub2T& l) ;
};

template < class R_ >
class Sub1
{
  typedef pair<typename R_::MainT> Rep;
  Rep base;
};

template < class R_ >
struct Sub2
{
  typedef pair<typename R_::MainT> Rep;
  Rep base;
};

struct Kernel
{
  typedef Main<Kernel> MainT;
  typedef Sub1<Kernel> Sub1T;
  typedef Sub2<Kernel> Sub2T;
};

Main<Kernel> f()
{
  return Main<Kernel> ();
}
