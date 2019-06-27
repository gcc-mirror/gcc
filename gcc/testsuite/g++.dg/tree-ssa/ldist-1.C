/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

#include <new>
struct T {
    int* p;
    T(T const&t):p(t.p){}
};
void f(T*__restrict a,T*__restrict b){
    for(int i=0;i<1024;++i){
	new(a+i)T(b[i]);
	b[i].~T();
    }
}

/* { dg-final { scan-tree-dump "generated memcpy" "ldist" } } */
