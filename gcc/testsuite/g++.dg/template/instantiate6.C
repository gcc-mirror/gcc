// { dg-do compile }

// Origin: gianni@mariani.ws
//	   Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/13289: ICE recursively instantiate static member data.

template <int N> struct S { 
    static const int C; 
}; 
 
template <int N> 
const int S<N>::C = S<(N+1)%2>::C;
 
template struct S<1>;
