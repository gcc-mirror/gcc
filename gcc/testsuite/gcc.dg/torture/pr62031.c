/* { dg-do run } */

#include <stdlib.h>

#define NUM_OF_STATES 4
typedef unsigned int entry_t[2];
typedef struct entries_item { entry_t metricEntries_[0]; } entries_item_t;

void __attribute__((noinline,noclone))
test_00(size_t numOfStates, entries_item_t* p_bm,
	const unsigned int* polyArray,
	size_t polyArraySize)
{
  size_t idx;
  unsigned int hlp0, hlp1;
  for (idx = 0; idx < numOfStates; ++idx)
    {
      size_t idy;

      hlp0 = (idx << 1) | 0x00;
      hlp1 = (idx << 1) | 0x01;
      p_bm->metricEntries_[idx][0] = 0;
      p_bm->metricEntries_[idx][1] = 0;
      for (idy = 0; idy < polyArraySize; ++idy)
	{
	  p_bm->metricEntries_[idx][0]
	      |= __builtin_parity(hlp0 & polyArray[idy]) << idy;
	  p_bm->metricEntries_[idx][1]
	      |= __builtin_parity(hlp1 & polyArray[idy]) << idy;
	}
    }
}

int main()
{
  unsigned int polyArray[] = { 0x07, 0x05 };
  entries_item_t* pBranchMetrics;
  pBranchMetrics = malloc(sizeof(entry_t) * NUM_OF_STATES);
  test_00(NUM_OF_STATES, pBranchMetrics, polyArray,
	  sizeof(polyArray) / sizeof(polyArray[0]));
  if (pBranchMetrics->metricEntries_[0][0] != 0
      || pBranchMetrics->metricEntries_[0][1] != 3
      || pBranchMetrics->metricEntries_[1][0] != 1
      || pBranchMetrics->metricEntries_[1][1] != 2
      || pBranchMetrics->metricEntries_[2][0] != 3
      || pBranchMetrics->metricEntries_[2][1] != 0
      || pBranchMetrics->metricEntries_[3][0] != 2
      || pBranchMetrics->metricEntries_[3][1] != 1)
    abort ();
  free(pBranchMetrics);
  return 0;
}
