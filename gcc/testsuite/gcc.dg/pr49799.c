/* PR rtl-optimization/49799 */
/* { dg-do assemble } */
/* { dg-options "-O2 -w" } */

static __inline int bar(int a)
{
    int tmp;

    if (a <= 0) a ^= 0xFFFFFFFF;

    return tmp - 1;
}

void foo(short *K)
{
    short tmp;
    short *pptr, P[14];

    pptr = P;
    tmp = bar(*K);
    *pptr = (*K << tmp) >> 16;

    if (*P < tmp)
        *K++ = 0;
}
