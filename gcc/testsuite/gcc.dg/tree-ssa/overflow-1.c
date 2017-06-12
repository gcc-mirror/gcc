/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

#if __SIZEOF_INT__ < 4
  __extension__ typedef __UINT32_TYPE__ uint32_t;
#else
  typedef unsigned uint32_t;
#endif

int f(uint32_t a){
    uint32_t b=5;
    uint32_t c=a-b;
    return c>a;
}
int g(uint32_t a){
    uint32_t b=32;
    uint32_t c=a+b;
    return c<a;
}

/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. <= 4;" "optimized" } } */
/* { dg-final { scan-tree-dump "a_\[0-9\]+.D. > 4294967263;" "optimized" } } */
