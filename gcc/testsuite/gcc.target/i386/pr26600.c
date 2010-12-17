/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -msse2" } */

void foo(int *p, int N)
{
    int i;
    for (i=0; i<8; ++i, ++p)
    {
        int j = N+2*(N+p[0]), k = 2*N+p[0];
        p[0] = j+N;
        p[5] = j+k;
    }
}

