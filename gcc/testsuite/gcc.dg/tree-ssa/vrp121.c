/* PR tree-optimization/89143 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error \\\(" "optimized" } } */

void link_error (void);

void
f1 (signed char i)
{
  if (__builtin_abs (i) < 0 || __builtin_abs (i) > __SCHAR_MAX__ + 1)
    link_error ();
}

void
f2 (signed char i)
{
  if (i < 0 || i > 15)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 0 || __builtin_abs (i) > 15)
    link_error ();
}

void
f3 (signed char i)
{
  if (i < 19 || i > 25)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 19 || __builtin_abs (i) > 25)
    link_error ();
}

void
f4 (signed char i)
{
  if (i > -60)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 60 || __builtin_abs (i) > __SCHAR_MAX__ + 1)
    link_error ();
}

void
f5 (signed char i)
{
  if (i < -__SCHAR_MAX__ || i > -30)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 30 || __builtin_abs (i) > __SCHAR_MAX__)
    link_error ();
}

void
f6 (signed char i)
{
  if (i < -__SCHAR_MAX__ || i > 30)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 0 || __builtin_abs (i) > __SCHAR_MAX__)
    link_error ();
}

void
f7 (signed char i)
{
  if (i < -31 || i > 30)
    __builtin_unreachable ();
  if (__builtin_abs (i) < 0 || __builtin_abs (i) > 31)
    link_error ();
}
