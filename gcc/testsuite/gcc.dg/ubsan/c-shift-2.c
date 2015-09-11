/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int
foo (int x)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case 1 >> -1: /* { dg-error "case label does not reduce to an integer constant" } */
    case -1 >> -1: /* { dg-error "case label does not reduce to an integer constant" } */
    case 1 << -1: /* { dg-error "case label does not reduce to an integer constant" } */
    case -1 << -1: /* { dg-error "case label does not reduce to an integer constant" } */
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
    case -1 >> 200: /* { dg-error "case label does not reduce to an integer constant" } */
    case 1 << 200: /* { dg-error "case label does not reduce to an integer constant" } */
      return 1;
    }
  return 0;
}
