/* Test the fix for PR92123.  */

/* Contributed by Vipul Parekh  <parekhvs@gmail.com>  */

#include <stdlib.h>
#include <stdio.h>
#include <ISO_Fortran_binding.h>

// Prototype for Fortran functions
extern void Fsub(CFI_cdesc_t *);

int main()
{
/* Note: ISO C forbids zero-size array 'dim' [-Wpedantic]
   Therefore, even though 'dat' represents a scalar, it is set rank 1/  */
  CFI_CDESC_T(1) dat;
  int irc = 0;

  irc = CFI_establish((CFI_cdesc_t *)&dat, NULL,
		      CFI_attribute_allocatable,
		      CFI_type_int, 0, (CFI_rank_t)0, NULL);
  if (irc != CFI_SUCCESS)
    {
      printf("CFI_establish failed: irc = %d.\n", irc);
      return EXIT_FAILURE;
    }

  Fsub((CFI_cdesc_t *)&dat);
  if (*(int *)dat.base_addr != 42)
    {
      printf("Fsub returned = %d.\n", *(int *)dat.base_addr);
      return EXIT_FAILURE;
    }

  irc = CFI_deallocate((CFI_cdesc_t *)&dat);
  if (irc != CFI_SUCCESS)
    {
      printf("CFI_deallocate for dat failed: irc = %d.\n", irc);
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
