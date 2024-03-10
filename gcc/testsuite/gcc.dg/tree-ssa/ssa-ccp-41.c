/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#if __SIZEOF_INT__ == 2
#define int __INT32_TYPE__
#endif

int foo(int x)
{
    int p = x & 24;
    int r = 1 << p; 
    return r & ((int)1<<17);
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
