/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O3 -fno-trapping-math -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-not "vect_recog_cond_expr_convert_pattern" "vect" } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 64 byte vectors" 1 "vect" { target { ! ia32 } } } } */

void
foo (float* a, float* b, float* c, float* d, double* __restrict e, int n)
{
    for (int i = 0 ; i != n; i++)
    {
        double tmp = c[i] + d[i];
        if (a[i] < b[i])
          tmp = 1.000000000000001;
        e[i] = tmp;
    }
}
