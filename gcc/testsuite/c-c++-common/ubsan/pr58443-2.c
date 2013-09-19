/* PR sanitizer/58443 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=unreachable,integer-divide-by-zero -w" } */

int
foo (int u, int o)
{
  return u >> o;
}

/* { dg-final { scan-assembler-not "__ubsan_handle_shift_out_of_bounds" } } */
