/* Test that __builtin_prefetch does no harm.

   Data prefetch should not fault if used with an invalid address.  */

#include <limits.h>

#define ARRSIZE 65
int *bad_addr[ARRSIZE];
int arr_used;

/* Fill bad_addr with a range of values in the hopes that on any target
   some will be invalid addresses.  */
void
init_addrs (void)
{
  int i;
  int bits_per_ptr = sizeof (void *) * 8;
  for (i = 0; i < bits_per_ptr; i++)
    bad_addr[i] = (void *)(1UL << i);
  arr_used = bits_per_ptr + 1;  /* The last element used is zero.  */
}

void
prefetch_for_read (void)
{
  int i;
  for (i = 0; i < ARRSIZE; i++)
    __builtin_prefetch (bad_addr[i], 0, 0);
}

void
prefetch_for_write (void)
{
  int i;
  for (i = 0; i < ARRSIZE; i++)
    __builtin_prefetch (bad_addr[i], 1, 0);
}

int
main ()
{
  init_addrs ();
  prefetch_for_read ();
  prefetch_for_write ();
  exit (0);
}
