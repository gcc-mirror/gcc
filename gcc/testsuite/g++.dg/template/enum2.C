// PR c++/15877

template <int n> struct T1 { enum { N = 3 }; };
template <int n> struct T2 { enum { N = n, N1 = T1<N>::N }; }; 
