/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int
foo (int x)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case 1 >> -1: /* { dg-error "operand of shift" } */
    case -1 >> -1: /* { dg-error "operand of shift" } */
    case 1 << -1: /* { dg-error "operand of shift" } */
    case -1 << -1: /* { dg-error "operand of shift" } */
      return 1;
    }
  return 0;
}

int
bar (int x)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case -1 >> 200: /* { dg-error "operand of shift" } */
    case 1 << 200: /* { dg-error "operand of shift" } */
      return 1;
    }
  return 0;
}
