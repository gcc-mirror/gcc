/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" }  */

_Bool
foo (_Bool a, _Bool b, _Bool c)
{
  _Bool r1 = a == 0 & b != 0;
  _Bool r2 = b != 0 & c == 0;
  return (r1 == 0 & r2 == 0);
}

/* { dg-final { scan-tree-dump-times " == " 0 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " != " 0 "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
