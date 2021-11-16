/* Verify that OpenACC device lowering executes with "-O0".  The actual
   logic in the test function does not matter. */

/* { dg-additional-options "-O0 -fdump-tree-oaccdevlow" } */

int main()
{

  int i, j;
  int ina[1024], out[1024], acc;

  for (j = 0; j < 32; j++)
    for (i = 0; i < 32; i++)
      ina[j * 32 + i] = (i == j) ? 2 : 0;

  acc = 0;
#pragma acc parallel loop copy(acc, ina, out)
      for (j = 0; j < 32; j++)
        {
#pragma acc loop reduction(+:acc)
	  for (i = 0; i < 32; i++)
              acc += ina[i];

	  out[j] = acc;
        }

  return 0;
}

/* { dg-final { scan-tree-dump ".omp_fn" "oaccdevlow3" } } */
