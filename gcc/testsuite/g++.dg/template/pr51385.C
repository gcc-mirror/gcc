// PR c++/51385

template <__SIZE_TYPE__ N> struct NTmpl;
template <typename T, typename U, typename V = NTmpl<sizeof(T *)> >
struct PtrConvs {
   enum { bad = 1 };
};

template <typename Target, typename Source>
struct PtrConvs<Target, Source, NTmpl<sizeof (*(Target **)0 = (Source *)0)> >;

template<typename T> struct test { static const bool value = true; };
template<> struct test<short> { static const bool value = false; };

template <typename T>
struct FussyTemplate
{
  int sa[test<T>::value ? 1 : -1];
};

struct B { };

typedef char chk[1];
typedef char chk[PtrConvs<FussyTemplate<short>, B>::bad];
