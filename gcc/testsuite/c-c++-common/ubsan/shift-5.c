/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int x;
int
foo (void)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case 1 >> -1:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 12 } */
/* { dg-error "is not a constant expression" "" { target c++ } 12 } */
    case -1 >> -1:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 15 } */
/* { dg-error "is not a constant expression" "" { target c++ } 15 } */
    case 1 << -1:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 18 } */
/* { dg-error "is not a constant expression" "" { target c++ } 18 } */
    case -1 << -1:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 21 } */
/* { dg-error "is not a constant expression" "" { target c++ } 21 } */
    case -1 >> 200:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 24 } */
/* { dg-error "is not a constant expression" "" { target c++ } 24 } */
    case 1 << 200:
/* { dg-error "case label does not reduce to an integer constant" "" {target c } 27 } */
/* { dg-error "is not a constant expression" "" { target c++ } 27 } */
      return 1;
    }
  return 0;
}
