// PR c++/15554

template <int n> struct T1 { enum { N = 3 }; };
template <int n> struct T2 { enum { N = 3, N1 = T1<N>::N }; };

