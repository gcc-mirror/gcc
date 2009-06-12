/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple-details-lineno" } */

void foo (int, int);

int
m(int x)
{
 int c, a;
 a = (c = 5) + 16 + x * 2 ;
 foo (c, a);
}

/* { dg-final { scan-tree-dump-times "10:9.*c = 5" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "10:14.*c . 16" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "10:4.*a =" 1 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
