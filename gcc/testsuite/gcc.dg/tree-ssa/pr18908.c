/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

_Bool f3(_Bool *p) { *p ^= 1; }

/* We should be able to canonicalize the above to use bitwise not.  */
/* { dg-final { scan-tree-dump "~D" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\\^ 1" "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
