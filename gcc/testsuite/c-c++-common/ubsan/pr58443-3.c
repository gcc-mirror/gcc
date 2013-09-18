/* PR sanitizer/58443 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -w" } */

int
foo (int u, int o)
{
  return u >> o;
}

int
bar (int u, int o)
{
  return u / o;
}

/* { dg-final { scan-assembler "__ubsan_handle_divrem_overflow" } } */
/* { dg-final { scan-assembler "__ubsan_handle_shift_out_of_bounds" } } */
