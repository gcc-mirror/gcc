/* { dg-do compile } */
/* { dg-options "-O2" } */

/* __builtin_stack_restore could generates {[%1:DI]=0;} in BLK mode,
   it could assert in try_const_anchors which only accepts SCALAR_INT.  */

long
foo (const int val)
{
  if (val == (0))
    return 0;
  void *p = __builtin_stack_save ();
  char c = val;
  __builtin_stack_restore (p);
  return c;
}
