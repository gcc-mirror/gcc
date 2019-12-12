/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */

// 'a' should disappear, but we are not there yet

int* f(int* a, int* b, int* c){
    __PTRDIFF_TYPE__ d = b - a;
    d += 1;
    return a + d;
}

/* { dg-final { scan-tree-dump-not "div" "optimized" } } */
