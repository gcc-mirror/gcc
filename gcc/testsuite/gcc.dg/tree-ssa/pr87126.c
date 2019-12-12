/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

int a, *b;

void f ()
{ 
  int d = 0, e = d;
  while (a++)
    ;
  if (e)
    goto L2;
L1:
  d = e;
  b = &d;
L2:
  if (d)
    goto L1;
}

/* The load of d could be eliminated if we'd value-number the
   irreducible region in RPO of the reducible result.  Likewise
   a redundant store could be removed.  */
/* { dg-final { scan-tree-dump-times "d = 0;" 1 "fre1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not " = d;" "fre1" { xfail *-*-* } } } */
