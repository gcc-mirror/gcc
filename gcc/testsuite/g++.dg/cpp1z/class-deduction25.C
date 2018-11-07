// Testcase from P0512R0 for C++17 NB comment US 19
// { dg-do compile { target c++17 } }

template<typename> struct remove_ref;
template<typename _Tp> struct remove_ref { typedef _Tp type; };
template<typename _Tp> struct remove_ref<_Tp&> { typedef _Tp type; };
template<typename _Tp> struct remove_ref<_Tp&&> { typedef _Tp type; };
template<typename _Tp> using remove_ref_t = typename remove_ref<_Tp>::type;

template<class T> struct A {
 A(T, int*); // #1
 A(A<T>&, int*); // #2
 enum { value };
};
template<class T, int N = remove_ref_t<T>::value> A(T&&, int*) -> A<T>; //#3

A a{1,0}; // uses #1 to deduce A<int> and initializes with #1
A b{a,0}; // uses #2 to deduce A<int> and initializes with #2

template <class,class> struct same;
template <class T> struct same<T,T> {};

same<decltype(a),A<int>> s1;
same<decltype(b),A<int>> s2;
