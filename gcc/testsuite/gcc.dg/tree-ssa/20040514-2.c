/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom3" } */
int
foo2 (distance, i, j)
     int distance;
     int i, j;
{
 int t = distance;
 if (t <= 0)
   t = ((t) >= 0 ? (t)  : -(t));
 return t;
}

/* There should be one ABS_EXPR and no conditionals.  */
/* { dg-final { scan-tree-dump-times "ABS_EXPR " 1 "dom3"} } */
/* { dg-final { scan-tree-dump-times "if " 0 "dom3"} } */

/* { dg-final { cleanup-tree-dump "dom3" } } */
