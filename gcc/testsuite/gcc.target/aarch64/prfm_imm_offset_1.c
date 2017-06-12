/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that we can generate the immediate-offset addressing
   mode for PRFM.  */

#define ARRSIZE 65
int *bad_addr[ARRSIZE];

void
prefetch_for_read (void)
{
  int i;
  for (i = 0; i < ARRSIZE; i++)
    __builtin_prefetch (bad_addr[i] + 2, 0, 0);
}

/* { dg-final { scan-assembler-times "prfm.*\\\[x\[0-9\]+, 8\\\]" 1 } } */
