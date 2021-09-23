// PR c++/77435

template<int, class T, T> struct S;
template<class T, T A> struct S<0, T, A> {};
int i;
S<0, int*, &i> r;  // OK
S<0, int&, i> s;   // error: aggregate 'S<0, int&, i> s' has incomplete type
