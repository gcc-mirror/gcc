/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ealias-graph" } */

struct S
{
  long m;
};

struct S
fn1 (struct S *a)
{
  if (a->m)
    a->m |= 2;
  return *a;
}
