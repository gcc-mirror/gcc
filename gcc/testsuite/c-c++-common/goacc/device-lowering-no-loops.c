/* Verify that OpenACC device lowering executes even if there are no OpenACC
   loops. */

/* { dg-additional-options "-O2 -fdump-tree-oaccdevlow" } */

int main()
{
  int x;
#pragma acc parallel copy(x)
  {
    asm volatile("");
  }

  return 0;
}

/* { dg-final { scan-tree-dump ".omp_fn" "oaccdevlow2" } } */
