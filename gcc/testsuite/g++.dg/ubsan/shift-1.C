/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int
foo (int x)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case 1 >> -1: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
    case -1 >> -1: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
    case 1 << -1: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
    case -1 << -1: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
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
    case -1 >> 200: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
    case 1 << 200: /* { dg-error "is not a constant expression" "" { xfail { *-*-* } } } */
      return 1;
    }
  return 0;
}
