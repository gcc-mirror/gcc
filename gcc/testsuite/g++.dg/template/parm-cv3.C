// CWG 1001/1322

// PR c++/101402
// PR c++/102033
// PR c++/102034
// PR c++/102039
// PR c++/102044

namespace test2{
template <class T>
void f(const T);

template<>
void f<int[]>(const int*){}
}

namespace test3{
template <class T>
struct A{
void f(T);
};

template<class T>
void A<T>::f(const T){}

template<>
void A<int[3]>::f(const int*){}
}

namespace test4 {
template<class TA>
struct A{
  template<class TB>
  struct B{
    typedef TB Arr3[3];
  };
};
template<class TA, class TB>
void f(const typename A<TA>::template B<TB>::Arr3){}
template <>
void f<int, char>(const typename A<int>::B<char>::Arr3){}
}

namespace test5
{
struct A{
  typedef int Arr3[3];
};

template<class T>
void f(const typename T::Arr3){}

template<>
void f<A>(const int[3]){}
}

namespace test6
{
struct A{
  typedef int Arr3[3];
};
template<class T>
void f(const typename T::Arr3){}
template<>
void f<A>(const int*){}
}

#if __cpp_alias_templates
namespace test7
{
template<class TA>
struct A{
  template<class TB>
  using Type=TB[3];
};
template<class TA, class TB>
void f(const typename A<TA>::template Type<TB>){}
template <>
void f<int, char>(const typename A<int>::template Type<char>){}
}
namespace test8
{
template<class TA>
struct A{
  template<class TB>
  struct B{
    using TB_Alias=TB;
    template<class TC=TB_Alias>
    struct C{
      typedef TC Arr3[3];
    };
  };
};
template<class TA, class TB>
void f(const typename A<TA>::template B<TB>::template C<>::Arr3){}
template <>
void f<int, char>(const typename A<int>::template B<char>::template C<>::Arr3){}
}
#endif

#if __cpp_decltype
namespace test0
{
template <class T>
struct A{
  T arr3[3];
};
template <class T>
void f(const decltype(A<T>::arr3)){}
template <>
void f<int>(const int[3]){}
}

#if __cpp_variable_templates
namespace test9
{
template<unsigned int N, class T>
void f(const T[N]){}

template<unsigned int N, class T>
using fPtr=decltype(f<N,T>)*;

template<unsigned int N, class T>
fPtr<N,T> af[N]={&f<N,T>};

template<unsigned int N, class T>
void g(const decltype(af<N,T>)){}

template<>
void g<1,int>(const fPtr<1,int>[1]){}
}
#endif
#endif

#if __cpp_concepts
template<class T>
concept IsLambdaAry3=__is_same(T, decltype(+[]{})[3]);
template<IsLambdaAry3 T>
void bar(const T){}
template<>
void bar<decltype(+[]{})[3]>(const decltype(+[]{})[3]){}
#endif
