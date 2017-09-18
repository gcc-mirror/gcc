/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm_short } */

void f(short*p){
  p=(short*)__builtin_assume_aligned(p,64);
  short*q=p+256;
  for(;p!=q;++p,--q){
    short t=*p;*p=*q;*q=t;
  }
}
void b(short*p){
  p=(short*)__builtin_assume_aligned(p,64);
  short*q=p+256;
  for(;p<q;++p,--q){
    short t=*p;*p=*q;*q=t;
  }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
