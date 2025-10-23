/* { dg-do compile } */
/* { dg-options "-O3 -funsafe-math-optimizations -mlasx -fno-unroll-loops -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "\.REDUC_PLUS" 4 "optimized" } } */

#define DEFINE_SUM_FUNCTION(T, FUNC_NAME, SIZE) \
T FUNC_NAME(const T arr[]) { \
    arr = __builtin_assume_aligned(arr, 64); \
    T sum = 0; \
    for (int i = 0; i < SIZE; i++) \
        sum += arr[i]; \
    return sum; \
}

DEFINE_SUM_FUNCTION (int, sum_int_1040, 1028)
DEFINE_SUM_FUNCTION (float, sum_float_1040, 1028)
DEFINE_SUM_FUNCTION (long, sum_long_1040, 1026)
DEFINE_SUM_FUNCTION (double, sum_double_1040, 1026)
