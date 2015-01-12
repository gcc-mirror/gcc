/* { dg-do compile } */
#include <assert.h>
void f(void*p,...){}
void g(void*p,long a,long b){assert(a==8);}
typedef struct
{
  int _mp_size;
  unsigned long *_mp_d;
} __mpz_struct;
typedef __mpz_struct mpz_t[1];
void h(mpz_t x) {
  x->_mp_d=0;
  x->_mp_size=0;
}
