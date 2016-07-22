/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(unsigned a){
    unsigned b=5;
    unsigned c=a-b;
    return c>a;
}
int g(unsigned a){
    unsigned b=32;
    unsigned c=a+b;
    return c<a;
}

/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. <= 4;" "optimized" } } */
/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. > 4294967263;" "optimized" } } */
