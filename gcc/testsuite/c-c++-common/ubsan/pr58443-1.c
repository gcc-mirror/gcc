/* PR sanitizer/58443 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=shift,unreachable -w" } */

int
foo (int u, int o)
{
  return u / o;
}

/* { dg-final { scan-assembler-not "__ubsan_handle_divrem_overflow" } } */
