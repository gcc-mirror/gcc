// { dg-do compile }
// { dg-options "-Os -fdump-tree-ch-details" }

extern unsigned int foo (int*) __attribute__((pure));

unsigned int
tr2 (int array[], int n)
{
  unsigned int sum = 0;
  int x;
  if (n > 0)
    for (x = 0; x < n; x++)
      sum += foo (&array[x]);
  return sum;
}

// { dg-final { scan-tree-dump-not "Not duplicating.*optimizing for size" "ch2" } }
