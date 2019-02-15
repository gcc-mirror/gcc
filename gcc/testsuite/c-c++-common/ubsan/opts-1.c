/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -fsanitize=shift -fsanitize=float-divide-by-zero -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-flto -fno-fat-lto-objects" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_divrem_overflow" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_shift_out_of_bounds" 1 "optimized" } } */

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
