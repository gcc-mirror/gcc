// PR c++/34846

template<typename, typename> struct __are_same { enum { __value = 0 }; };
template<typename _Tp> struct __are_same<_Tp, _Tp> { enum { __value = 1 }; };
template<typename, bool> struct __enable_if { };
template<typename _Tp> struct __enable_if<_Tp, true> { typedef _Tp __type; };
template<typename _Iterator, typename _Container> class __normal_iterator {
public:
  __normal_iterator();
  template<typename _Iter>
  __normal_iterator(
    const __normal_iterator<_Iter, typename __enable_if<_Container,
(__are_same<_Iter, typename _Container::pointer>::__value) >::__type>& __i)
  { }
};
template<typename _Tp> class vector {
public:
  typedef _Tp* pointer;
  typedef __normal_iterator<int, vector<_Tp> > iterator;
};
void test() {
  typedef int t;
  vector<t*>::iterator x;
  vector<t*>::iterator y = x;
}
