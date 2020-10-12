/* { dg-do compile } */
/* { dg-options "-O -fwrapv -fdump-tree-gimple" } */

typedef __INT32_TYPE__ int32_t;
typedef unsigned __INT32_TYPE__ uint32_t;

int e(int32_t x){return 3*x==5;}
int f(int32_t x){return 3*x==-5;}
int g(int32_t x){return -3*x==5;}
int h(int32_t x){return 7*x==3;}
int i(uint32_t x){return 7*x==3;}

/* { dg-final { scan-tree-dump-times "== 1431655767" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "== -1431655767" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "== 613566757" 2 "gimple" } } */
