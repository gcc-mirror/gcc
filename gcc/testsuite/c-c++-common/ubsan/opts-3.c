/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -fno-sanitize=shift -fno-sanitize=float-divide-by-zero -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_divrem_overflow" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "__ubsan_handle_shift_out_of_bounds" "optimized" } } */

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

float
baz (float x, float y)
{
  return x / y;
}
