/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-esra-details" } */

typedef struct teststruct
{
  double d;
  int i1;
  char c1;
  float z;
  char c2;
  int i2;
} teststruct;


void cow (int i)
{
  teststruct a, b, c, d;

  a.d = 3.2;
  a.i1 = i;

  b = a;
  c = b;
  d = c;

  if (d.i1 != i)
    link_error ();
}


/* Suaccesses of b and c should have been created.  */
/* { dg-final { scan-tree-dump "expr = b.d"  "esra"} } */
/* { dg-final { scan-tree-dump "expr = b.i1"  "esra"} } */
/* { dg-final { scan-tree-dump "expr = c.d"  "esra"} } */
/* { dg-final { scan-tree-dump "expr = c.i1"  "esra"} } */
/* { dg-final { cleanup-tree-dump "esra" } } */

/* There should be no reference to link_error.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
