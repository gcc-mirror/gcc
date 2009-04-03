/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fno-tree-sra -fdump-tree-fre-details" } */

/* Should be optimized, propagating &a into (*p)[i].  */

/* For this testcase we need TBAA to work.  */

struct Foo
{
  void *data;
  int size;
};
void foo(double (*q)[4], struct Foo *tmp1)
{
  double a[4];
  int i;
  tmp1->data = &a;
  tmp1->size = 4;
  for (i=0; i<4; ++i)
    {
      double (*p)[4] = tmp1->data;
      (*p)[i] = (*q)[i];
    }
}

/* { dg-final { scan-tree-dump "Inserted .* &a" "fre" } } */
/* { dg-final { scan-tree-dump "Replaced tmp1_.\\\(D\\\)->data" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
