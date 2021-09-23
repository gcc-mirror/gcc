// PR c++/96555

template<class T, int i>
struct X;

template<class T>
struct X<T, sizeof(T)> {};

X<int, sizeof(int)> x1;
X<int, sizeof(int)+1> x2; // { dg-error "incomplete" }


struct A { int x; } a;
template<int, int> struct B;
template<int y>
struct B<y, sizeof(a.x)> { };

B<0, sizeof(int)> b1;
B<0, sizeof(int)+1> b2; // { dg-error "incomplete" }
