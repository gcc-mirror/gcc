/* { dg-do run } */
/* Check that find_scan_insn properly handles swapped FP comparisons.  */
static double x;
static int exit_code;

void __attribute__ ((noinline))
check_int (int a, int b)
{
  exit_code += (a != b);
}

int
main (void)
{
  x = 0.0;
  asm ("" : "+m" (x));
  check_int (__builtin_isgreater (x, 1.0), 0);
  check_int (__builtin_isgreaterequal (x, 1.0), 0);
  check_int (__builtin_isless (x, 1.0), 1);
  check_int (__builtin_islessequal (x, 1.0), 1);
  check_int (__builtin_islessgreater (x, 1.0), 1);
  return exit_code;
}
