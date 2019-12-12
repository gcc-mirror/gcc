/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -fno-sanitize-recover=integer-divide-by-zero -fno-sanitize-recover=shift -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_divrem_overflow_abort" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_shift_out_of_bounds_abort" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_type_mismatch_v1" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "__ubsan_handle_type_mismatch_v1_abort" "optimized" } } */

int
foo (int x, int y)
{
  return x / y;
}

int
bar (int x, int y)
{
  return x << y;
}

enum E { E0, E1, E2, E3 };

enum E
baz (enum E *x)
{
  return *x;
}
