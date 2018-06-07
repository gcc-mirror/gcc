/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm_short } */

void b(short*p){
  p=(short*)__builtin_assume_aligned(p,64);
  short*q=p+255;
  for(;p<q;++p,--q){
    short t=*p;*p=*q;*q=t;
  }
}
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
