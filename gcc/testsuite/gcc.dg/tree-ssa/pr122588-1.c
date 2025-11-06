/* { dg-do compile } */
/* { dg-options "-O1 -fgimple" } */
/* PR tree-optimization/122588 */

/* The removal of unreachable blocks should not
   change blocks which have already become true/false.
   The function below was is an example of that. And
   forwprop does not go into non-executable blocks
   so the statement `t = _1;` was still holding the
   old reference.  */

int t;

__GIMPLE(ssa,startwith("forwprop4")) void g(void)
{
  int _1;
  __BB(2):
  _1 = 1;
  if (_1 != 0)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  __builtin_unreachable ();

  __BB(4):
  t = _1;
  return;
}
