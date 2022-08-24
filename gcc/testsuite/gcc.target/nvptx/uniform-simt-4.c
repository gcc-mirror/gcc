/* { dg-do compile } */
/* { dg-options "-O2 -muniform-simt -mptx=_" } */

enum memmodel
{
  MEMMODEL_RELAXED = 0
};

unsigned long long int *p64;
unsigned long long int v64;

int
main()
{
  __atomic_fetch_add (p64, v64, MEMMODEL_RELAXED);

  return 0;
}

/* { dg-final { scan-assembler-times "atom.add.u64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "bar.warp.sync" 1 } } */
/* { dg-final { scan-assembler-not "shfl.sync.idx" } } */
