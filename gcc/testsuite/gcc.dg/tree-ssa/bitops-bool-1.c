/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-forwprop1" } */
/* PR tree-optimized/113186 */

_Bool f(_Bool a, _Bool c)
{
  _Bool b = (a^c);
  _Bool d = (a^!c);
  return b & d;
}

/* This function should be optimized to return 0; */
/* { dg-final { scan-tree-dump "return 0" "optimized" } } */
/* { dg-final { scan-tree-dump "return 0" "forwprop1" } } */
