/* { dg-do compile } */
/* { dg-options "-O -fno-tree-forwprop -fdump-tree-fre1" } */

const int a[]={1,2,3};
int f(){
    int*b=__builtin_malloc(12);
    __builtin_memcpy(b,a,12);
    return b[0];
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
