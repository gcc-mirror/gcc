/* { dg-do compile } */
/* { dg-options "-Ofast -march=armv8-a+sve -mmax-vectorization -fdump-tree-vect-details" } */

void
foo (char *restrict a, int *restrict b, int *restrict c,
     int *restrict d, int stride)
{
    if (stride <= 1)
        return;

    for (int i = 0; i < 3; i++)
        {
            int res = c[i];
            int t = b[i * stride];
            if (a[i] != 0)
                res = t * d[i];
            c[i] = res;
        }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
