/* { dg-do compile } */
/* { dg-require-effective-target float16 } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */
/* { dg-add-options float16 } */
/* { dg-final { scan-tree-dump-times " = ABS_EXPR <x_\[0-9]*\\\(D\\\)>;" 1 "optimized" } } */

_Float16  absfloat16(_Float16 x) {
    if (x < 0.0f) {
        return -x;
    } else {
        return x;
    }
}

