// Build don't link: 

template <class X> struct A {  };
struct A<int> { int foo (); };
int A<int>::foo () { return 37; }
