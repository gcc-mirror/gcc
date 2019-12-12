/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized-raw" } */

// ldist produces (((q-p-4)/4)&...+1)*4
// Make sure we remove at least the division
// Eventually this should just be n*4

void foo(int*p, __SIZE_TYPE__ n){
  for(int*q=p+n;p!=q;++p)*p=0;
}

/* { dg-final { scan-tree-dump "builtin_memset" "optimized" } } */
/* { dg-final { scan-tree-dump-not "div" "optimized" } } */
