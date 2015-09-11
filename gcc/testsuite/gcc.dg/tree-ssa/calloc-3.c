/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void*f(){
    char*p=__builtin_malloc(42);
    __builtin_memset(p,0,42);
    __builtin_memset(p,0,42);
    return p;
};

/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { scan-tree-dump-times "calloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */
