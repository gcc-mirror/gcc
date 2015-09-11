/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-expand" } */

extern void abort (void);
unsigned int a = 1;

int
main (void)
{
  unsigned int b, c, d;

  if (sizeof (int) != 4 || (int) 0xc7d24b5e > 0)
    return 0;

  c = 0xc7d24b5e;
  d = a | -2;
  b = (d == 0) ? c : (c % d);
  if (b != c)
    abort ();

  return 0;
}

/* { dg-final { scan-rtl-dump "expr_list:REG_EQUAL \\(const_int -942519458" "expand" } } */
