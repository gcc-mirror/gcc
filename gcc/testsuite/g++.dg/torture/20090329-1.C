/* { dg-do compile } */
/* { dg-additional-options "-Wno-return-type" } */

struct input_iterator_tag { };
template<typename _Category, typename _Tp, typename _Distance = long, typename _Pointer = _Tp*, typename _Reference = _Tp&>
struct iterator {
    typedef _Category iterator_category;
};
template<typename _Iterator> struct iterator_traits {
    typedef typename _Iterator::iterator_category iterator_category;
};
template<typename, typename> struct __lc_rai {
    template<typename _II1, typename _II2>
	static _II1 __newlast1(_II1, _II1 __last1, _II2, _II2) {
	    return __last1;
	}
    template<typename _II> 
	static bool __cnd2(_II __first, _II __last) {
	    return __first != __last;
	}
};
template<typename _II1, typename _II2, typename _Compare>
bool lexicographical_compare(_II1 __first1, _II1 __last1, _II2 __first2,
			     _II2 __last2, _Compare __comp) {
    typedef typename iterator_traits<_II1>::iterator_category _Category1;
    typedef typename iterator_traits<_II2>::iterator_category _Category2;
    typedef __lc_rai<_Category1, _Category2> __rai_type;
    __last1 = __rai_type::__newlast1(__first1, __last1, __first2, __last2);
    for (;
	 __first1 != __last1 && __rai_type::__cnd2(__first2, __last2);
	 ++__first1, ++__first2) {
	if (__comp(*__first1, *__first2)) return true;
    }
}
void __assert_fail () throw () __attribute__ ((__noreturn__));
template<typename T> struct BoundsContainer { };
template<class T> class input_iterator_wrapper : public iterator<input_iterator_tag, T, long, T*, T&> {
public:
    typedef BoundsContainer<T> ContainerType;
    T* ptr;
    ContainerType* SharedInfo;
    input_iterator_wrapper(const input_iterator_wrapper& in) : ptr(in.ptr), SharedInfo(in.SharedInfo) { }
    bool operator==(const input_iterator_wrapper& in) const {
	(static_cast<void> ((SharedInfo != __null
			     && SharedInfo == in.SharedInfo)
			    ? 0 : (__assert_fail (), 0)));
    }
    bool operator!=(const input_iterator_wrapper& in) const {
	return !(*this == in);
    }
    T& operator*() const { }
    input_iterator_wrapper& operator++() { }
};
struct X { };
bool predicate(const X&, const X&) {
    return true;
}
bool test2(input_iterator_wrapper<X>& x) {
    return lexicographical_compare(x, x, x, x, predicate);
}
