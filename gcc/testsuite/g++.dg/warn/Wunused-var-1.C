// { dg-options "-Wunused" }
template <int> struct X { static const int s = 2; };
template <typename T> int f() { const int v = 2; return X<v+1>::s; }
template <typename T> int g() { const int v = 2; return X<v>::s; }
template <typename T> int h() { const int v = 2; return X<1 ? v : 0>::s; }
template int f<int>();
template int g<int>();
template int h<int>();
