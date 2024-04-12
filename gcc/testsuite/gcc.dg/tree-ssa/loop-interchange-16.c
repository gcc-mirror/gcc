/* PR/101280 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-linterchange-details" } */

void dummy (double *, double *);
#define LEN_2D 32
double aa[LEN_2D][LEN_2D], bb[LEN_2D][LEN_2D];
double s231(int iterations)
{
//    loop interchange
//    loop with data dependency
    for (int nl = 0; nl < 100*(iterations/LEN_2D); nl++) {
        for (int i = 0; i < LEN_2D; ++i) {
#pragma GCC unroll 0
            for (int j = 1; j < LEN_2D; j++) {
                aa[j][i] = aa[j - 1][i] + bb[j][i];
            }
        }
        dummy(aa[0],bb[0]);
    }
}

/* { dg-final { scan-tree-dump "loops interchanged" "linterchange" } } */
