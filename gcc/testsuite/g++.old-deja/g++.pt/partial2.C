// Build don't link:
// Tests partial specialization
template<class T> struct foo1 {};
template<class T, int n> struct foo1<T[n]>;
foo1<char> bar1;
foo1<char[10]> baz1; // ERROR - incomplete type

template<class T> struct foo2 {};
template<class T, unsigned n> struct foo2<T[n]>;
foo2<char> bar2;
foo2<char[10]> baz2; // ERROR - incomplete type

typedef unsigned int other1_t;
template<class T> struct foo3 {};
template<class T, other1_t n> struct foo3<T[n]>;
foo3<char> bar3;
foo3<char[10]> baz3; // ERROR - incomplete type - 

typedef int other2_t;
template<class T> struct foo4 {};
template<class T, other1_t n> struct foo4<T[n]>;
foo4<char> bar4;
foo4<char[10]> baz4; // ERROR - incomplete type - 
