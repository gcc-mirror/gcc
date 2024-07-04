/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void foo(int * restrict a, int * restrict b, int n){
  int i;
  for(i=0; i<n; ++i){
    a[i] = a[i] + b[i];
    __builtin_prefetch(&(b[i+8]));
  }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  } } */
