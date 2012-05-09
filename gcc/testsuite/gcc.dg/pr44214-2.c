/* { dg-do compile } */
/* { dg-options "-O2 -freciprocal-math -fdump-tree-original" } */

void do_div (_Complex double *a, _Complex double *b)
{
  *a = *b / (4.0 - 5.0fi);
}

/* Constant folding should multiply *b by the reciprocal of 4 - 5i
   = 4/41 + (5/41)i.  */

/* { dg-final { scan-tree-dump-times " \\\* " 1 "original" } } */
/* { dg-final { scan-tree-dump-times " / " 0 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
