// Testcase from P0512R0 for C++17 NB comment US 20
// { dg-do compile { target c++17 } }

template <class,class> struct same;
template <class T> struct same<T,T> {};

template<class T> struct A {
  template<class U>
  A(T&&, U&&, int*); // #1: T&& is not a forwarding reference
                     //     U&& is a forwarding reference
  A(T&&, int*);	     // #2
};
template<class T>
A(T&&, int*) -> A<T>; // #3: T&& is a forwarding reference

int i;
int *ip;
A a0{0, 0, ip}; // uses #1 to deduce A<int> and #1 to initialize
same<decltype(a0),A<int>> s1;
A a2{i, ip};    // uses #3 to deduce A<int&> and #2 to initialize
same<decltype(a2),A<int&>> s2;

A a{i, 0, ip};  // { dg-error "" } cannot deduce from #1
