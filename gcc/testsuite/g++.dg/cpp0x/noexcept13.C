// PR c++/49107
// { dg-options -std=c++11 }

namespace std
{
  template<typename _Tp> _Tp&& declval() noexcept;

  struct true_type { static const bool value = true; };
  struct false_type { static const bool value = false; };

  template<typename _Tp, typename _Arg>
    struct __is_direct_constructible_impl
    {
      template<typename _Tp2, typename _Arg2, typename
	       = decltype(::new _Tp2(declval<_Arg2>()))>
      static true_type __test(int);

      template<typename, typename>
      static false_type __test(...);

      typedef decltype(__test<_Tp, _Arg>(0)) type;
    };

  template<typename _Tp, typename _Arg>
    struct __is_direct_constructible_new_safe
    : public __is_direct_constructible_impl<_Tp, _Arg>::type
    { };

  template<class _T1, class _T2>
    struct pair
    {
      pair() = default;
      constexpr pair(const pair&) = default;

      pair(pair&& __p)
      noexcept(__is_direct_constructible_new_safe<_T2,_T2&&>::value);
    };
}

template <class R_>
struct Vector3
{
  typedef typename R_::Ray_3 Ray_3;
  Vector3() {}
  explicit Vector3(const Ray_3& r);
};

template < class R_ > class LineC3
{
  typedef typename R_::Vector_3 Vector_3;
  std::pair<int, Vector_3> x;
};

template < class R_ > class RayH3
{
  typedef typename R_::Vector_3 Vector_3;
  std::pair<int, Vector_3> x;
};

template <typename Kernel >
struct Homogeneous_base
{
  typedef LineC3<Kernel> Line_3;
  typedef RayH3<Kernel> Ray_3;
};

template < typename RT_>
struct Simple_homogeneous
: public Homogeneous_base< Simple_homogeneous<RT_> >
{
  typedef Vector3<Simple_homogeneous<RT_> > Vector_3;
};

int main()
{
  typedef Simple_homogeneous<double> R;
  R::Line_3 l3;
}
