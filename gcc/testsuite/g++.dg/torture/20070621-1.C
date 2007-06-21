/* Reduced from libstdc++-v3/testsuite/25_algorithms/equal/1.cc

1.2.ii: In function 'void test1()':
1.2.ii:104: error: true/false edge after a non-COND_EXPR in bb 15
1.2.ii:104: internal compiler error: verify_flow_info failed

*/

typedef long int ptrdiff_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
    template<typename, typename>     struct __are_same     {
        enum {
  __value = 0 };
      };
    template<typename _Tp>     struct __is_integer     {
        enum {
  __value = 0 };
      };
    template<typename _Tp>     struct __is_pointer     {
        enum {
  __value = 0 };
      };
    template<typename _Tp>     struct __is_normal_iterator     {
        enum {
  __value = 0 };
      };
    struct input_iterator_tag {
 };
    template<typename _Category, typename _Tp, typename _Distance = ptrdiff_t,            typename _Pointer = _Tp*, typename _Reference = _Tp&>     struct iterator     {
        typedef _Tp value_type;
      };
    template<typename _Iterator>     struct iterator_traits     {
        typedef typename _Iterator::value_type value_type;
      };
    template<typename _Iterator,     bool _BoolType = __is_normal_iterator<_Iterator>::__value>     struct __niter_base     {
        static const _Iterator&       __b(const _Iterator& __it)       {
  return __it;
  }
      };
    template<bool _BoolType>     struct __equal     {
        template<typename _II1, typename _II2>         static bool         equal(_II1 __first1, _II1 __last1, _II2 __first2)         {
     for (;
  __first1 != __last1;
  ++__first1, ++__first2)      if (!(*__first1 == *__first2))        return false;
   }
      };
    template<typename _II1, typename _II2>     inline bool     __equal_aux(_II1 __first1, _II1 __last1, _II2 __first2)     {
        typedef typename iterator_traits<_II1>::value_type _ValueType1;
        typedef typename iterator_traits<_II2>::value_type _ValueType2;
        const bool __simple = (__is_integer<_ValueType1>::__value                       && __is_pointer<_II1>::__value                       && __is_pointer<_II2>::__value         && __are_same<_ValueType1, _ValueType2>::__value);
        return std::__equal<__simple>::equal(__first1, __last1, __first2);
      }
    template<typename _II1, typename _II2>     inline bool     equal(_II1 __first1, _II1 __last1, _II2 __first2)     {
        return std::__equal_aux(__niter_base<_II1>::__b(__first1),          __niter_base<_II1>::__b(__last1),          __niter_base<_II2>::__b(__first2));
      }
  }
extern "C" {
  extern void __assert_fail (__const char *__assertion, __const char *__file,       unsigned int __line, __const char *__function)      throw () __attribute__ ((__noreturn__));
  }
namespace __gnu_test {
    template<typename T>     struct BoundsContainer     {
        T* first;
        T* last;
        BoundsContainer(T* _first, T* _last)  : first(_first), last(_last)       {
  }
      };
    template<class T>   class input_iterator_wrapper:public std::iterator   <std::input_iterator_tag, T, ptrdiff_t, T*, T&>   {
    public:
      typedef BoundsContainer<T> ContainerType;
      T* ptr;
      ContainerType* SharedInfo;
      input_iterator_wrapper(T* _ptr, ContainerType* SharedInfo_in)       : ptr(_ptr), SharedInfo(SharedInfo_in)     {
  }
      bool     operator==(const input_iterator_wrapper& in) const     {
        (static_cast<void> (__builtin_expect (!!(SharedInfo != __null && SharedInfo == in.SharedInfo), 1) ? 0 : (__assert_fail ("SharedInfo != __null && SharedInfo == in.SharedInfo", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/util/testsuite_iterators.h", 201, __PRETTY_FUNCTION__), 0)));
        (static_cast<void> (__builtin_expect (!!(ptr>=SharedInfo->first && in.ptr>=SharedInfo->first), 1) ? 0 : (__assert_fail ("ptr>=SharedInfo->first && in.ptr>=SharedInfo->first", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/util/testsuite_iterators.h", 202, __PRETTY_FUNCTION__), 0)));
      }
      bool     operator!=(const input_iterator_wrapper& in) const     {
        return !(*this == in);
      }
      T&     operator*() const     {
        (static_cast<void> (__builtin_expect (!!(SharedInfo && ptr < SharedInfo->last), 1) ? 0 : (__assert_fail ("SharedInfo && ptr < SharedInfo->last", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/util/testsuite_iterators.h", 215, __PRETTY_FUNCTION__), 0)));
        (static_cast<void> (__builtin_expect (!!(ptr >= SharedInfo->first), 1) ? 0 : (__assert_fail ("ptr >= SharedInfo->first", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/util/testsuite_iterators.h", 216, __PRETTY_FUNCTION__), 0)));
      }
      input_iterator_wrapper&     operator++()     {
        (static_cast<void> (__builtin_expect (!!(SharedInfo && ptr < SharedInfo->last), 1) ? 0 : (__assert_fail ("SharedInfo && ptr < SharedInfo->last", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/util/testsuite_iterators.h", 237, __PRETTY_FUNCTION__), 0)));
        ptr++;
        SharedInfo->first=ptr;
      }
    };
    template <class T, template<class T> class ItType>   struct test_container   {
      typename ItType<T>::ContainerType bounds;
      test_container(T* _first, T* _last):bounds(_first, _last)     {
  }
      ItType<T>     it(T* pos)     {
        return ItType<T>(pos, &bounds);
      }
      ItType<T>     begin()     {
  return it(bounds.first);
  }
      ItType<T>     end()     {
  }
     };
  }
using __gnu_test::test_container;
using __gnu_test::input_iterator_wrapper;
typedef test_container<int, input_iterator_wrapper> Container;
int array1[] = {
 0, 1};
int array2[] = {
 1, 0};
void test1() {
    Container con1(array1, array1);
    Container con2(array2, array2);
    (static_cast<void> (__builtin_expect (!!(std::equal(con1.begin(), con1.end(), con2.begin())), 1) ? 0 : (__assert_fail ("std::equal(con1.begin(), con1.end(), con2.begin())", "/abuild/rguenther/gcc/libstdc++-v3/testsuite/25_algorithms/equal/1.cc", 35, __PRETTY_FUNCTION__), 0)));
  }
