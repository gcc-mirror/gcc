/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE() void fn1()
{
  if (1)
    goto
  else /* { dg-error "expected label" } */
    goto lbl;
}

__GIMPLE() void fn2()
{
  if (1)
    goto lbl;
  else
    goto ; /* { dg-error "expected label" } */
}

__GIMPLE() void fn3()
{
  if (1)
    goto lbl;
  else
    goto
} /* { dg-error "expected label" } */

