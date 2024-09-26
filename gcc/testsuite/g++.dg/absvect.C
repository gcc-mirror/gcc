/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-phiopt1" } */
/* { dg-final { scan-tree-dump-times " = ABS_EXPR <x_\[0-9]*\\\(D\\\)>;" 1 "phiopt1" } } */

typedef int v2si __attribute__ ((vector_size (2 * sizeof(int))));
typedef short v2hi __attribute__ ((vector_size (2 * sizeof(short))));

v2hi  absvect1 (v2hi x, int i) {
    v2hi neg = -x;
    return (x > 0) ? x : neg;
}

