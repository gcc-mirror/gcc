/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */
/* { dg-shouldfail "ubsan" } */

int
foo (int x)
{
  /* None of the following should pass.  */
  switch (x)
    {
    case 1 >> -1:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 11 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 11 } */
    case -1 >> -1:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 14 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 14 } */
    case 1 << -1:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 17 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 17 } */
    case -1 << -1:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 20 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 20 } */
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
    case -1 >> 200:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 34 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 34 } */
    case 1 << 200:
/* { dg-error "case label does not reduce to an integer constant" "" { target c } 37 } */
/* { dg-error "is not a constant expression" "" { xfail { *-*-* } } 37 } */
      return 1;
    }
  return 0;
}
