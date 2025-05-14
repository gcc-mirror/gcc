/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not { >> } "optimized" } } */
/* { dg-final { scan-tree-dump-not { \* } "optimized" } } */

unsigned int
f1 (unsigned int x)
{
    x >>= 1;
    unsigned long y = x;
    y &= 255;
    x = y;
    x *= 2;
    return x;
}

unsigned int
f2 (unsigned int x)
{
    x >>= 1;
    unsigned long y = x;
    y &= -2UL;
    x = y;
    x *= 2;
    return x;
}

unsigned int
f3 (unsigned int x)
{
    x >>= 1;
    unsigned short y = x;
    y &= 255;
    x = y;
    x *= 2;
    return x;
}

unsigned int
f4 (unsigned int x)
{
    x >>= 1;
    short y = x;
    y &= (unsigned short) ~0U >> 1;
    x = y;
    x *= 2;
    return x;
}

unsigned int
f5 (unsigned int x)
{
    x >>= 16;
    short y = x;
    y &= -2;
    x = y;
    x *= 1 << 16;
    return x;
}
