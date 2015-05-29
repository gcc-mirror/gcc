/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmlower" } */

char array[4];

void *memset(void *s, int c, __SIZE_TYPE__);

int main()
{
  __transaction_atomic {
    memset(array, 'b', sizeof(4));
  }
  return 0;
}

/* { dg-final { scan-tree-dump-times "GTMA_HAVE_STORE" 1 "tmlower" } } */
