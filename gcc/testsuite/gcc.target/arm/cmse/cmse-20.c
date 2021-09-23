/* This test is executed only if the execution engine supports CMSE instructions.  */
/* { dg-options "--save-temps -mcmse -Wl,--section-start,.gnu.sgstubs=0x00400000" } */

#include <arm_cmse.h>
#include <stdlib.h>
#include <stdio.h>

void __attribute__((cmse_nonsecure_entry))
secure_fun (int a, int *p)
{
  void *b = cmse_check_address_range ((void *)p, a, 1);

  if (b == NULL)
   __builtin_abort ();
  printf("%d", *((int *)b));
}

int
main (void)
{
  int *ptr;
  int size = 1;
  ptr = (int *) calloc (1, sizeof(int *));
  *ptr = 1315852292;
  secure_fun (size, ptr);
  free (ptr);
  return 0;
}
