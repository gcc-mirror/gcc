/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-vops" } */

struct { int a, b; } soup1, soup2;
void
foo ()
{
  soup1 = soup2;
  soup1.a = 66;
  soup1.b = 77;
}

/* We should eliminate the first assignment.  */
/* { dg-final { scan-tree-dump-times "VDEF" 2 "dse1" } } */
