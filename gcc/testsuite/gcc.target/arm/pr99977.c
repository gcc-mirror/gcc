/* { dg-do compile } */
/* { dg-options "-march=armv8-m.base -mfloat-abi=soft -O2" } */
_Bool f1(int *p) { return __sync_bool_compare_and_swap (p, -1, 2); }
_Bool f2(int *p) { return __sync_bool_compare_and_swap (p, -8, 2); }
int g1(int *p) { return __sync_val_compare_and_swap (p, -1, 2); }
int g2(int *p) { return __sync_val_compare_and_swap (p, -8, 3); }
