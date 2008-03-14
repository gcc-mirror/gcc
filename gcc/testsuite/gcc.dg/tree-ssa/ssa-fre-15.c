/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra --param max-aliased-vops=0 --param max-fields-for-field-sensitive=0 -fdump-tree-fre-details" } */

/* Should be optimized, propagating &a into (*p)[i] with parameters
     --param max-aliased-vops=0 --param max-fields-for-field-sensitive=0
   which means max 1 VOP per stmt and no SFTs.  */

struct Foo
{
  void *data;
  double size;
};
void foo(double (*q)[4])
{
  struct Foo tmp1;
  double a[4];
  int i;
  tmp1.data = &a;
  for (i=0; i<4; ++i)
    {
      double (*p)[4] = tmp1.data;
      (*p)[i] = (*q)[i];
      /* We want a PHI for the VOP for accessing tmp1.data, so place
 	 this store to tmp1 here.  */
      tmp1.size -= 1.0;
    }
}

/* { dg-final { scan-tree-dump "Replaced" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
