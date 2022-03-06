/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE (ssa) void
bar (void)
{
  int _3;

__BB(2):
  return;
} /* { dg-error "version 3 has no definition" } */
