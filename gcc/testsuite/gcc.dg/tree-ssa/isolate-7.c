/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-isolate-paths" } */

extern int oof ();
extern int x;
_Bool
gcd_of_steps_may_divide_p ()
{
  long cd = 0, val;
  if (x)
    cd = oof ();
  return val % cd == 0;
}
/* { dg-final { scan-tree-dump-times "__builtin_trap" 1 "isolate-paths"} } */

