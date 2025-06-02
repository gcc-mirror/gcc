/* { dg-do compile } */
void
foo (int* a, int* restrict b)
{
    b[0] = a[0] * a[64];
    b[1] = a[65] * a[1];
    b[2] = a[2] * a[66];
    b[3] = a[67] * a[3];
    b[4] = a[68] * a[4];
    b[5] = a[69] * a[5];
    b[6] = a[6] * a[70];
    b[7] = a[7] * a[71];
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp2" { target vect_int_mult } } } */
