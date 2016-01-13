/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dom2-details-blocks" } */
/* We want to verify no outgoing edge from a conditional
   has a probability of 100%.  */
/* { dg-final { scan-tree-dump-not "succ:\[ \]+. .100.0%.  .\(TRUE|FALSE\)_VALUE" "dom2"} } */


void (*zend_block_interruptions) (void);

int * _zend_mm_alloc_int (int * heap, long int size)
{
  int *best_fit;
  long int true_size = (size < 15 ? 32 : size);

  if (zend_block_interruptions)
    zend_block_interruptions ();

  if (__builtin_expect ((true_size < 543), 1))
    best_fit = heap + 2;
  else
    best_fit = heap;

  return best_fit;
}

