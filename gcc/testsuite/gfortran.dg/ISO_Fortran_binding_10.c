/* Test the fix of PR89843.  */

/* Contributed by Reinhold Bader  <Bader@lrz.de> */

#include "../../../libgfortran/ISO_Fortran_binding.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

void sa(CFI_cdesc_t *, int, int *);

void si(CFI_cdesc_t *this, int flag, int *status)
{
  int value, sum;
  bool err;
  CFI_CDESC_T(1) that;
  CFI_index_t lb[] = { 0, 0 };
  CFI_index_t ub[] = { 4, 0 };
  CFI_index_t st[] = { 2, 0 };
  int chksum[] = { 9, 36, 38 };

  if (flag == 1)
    {
      lb[0] = 0; lb[1] = 2;
      ub[0] = 2; ub[1] = 2;
      st[0] = 1; st[1] = 0;
    }
  else if (flag == 2)
    {
      lb[0] = 1; lb[1] = 0;
      ub[0] = 1; ub[1] = 3;
      st[0] = 0; st[1] = 1;
    }

  CFI_establish((CFI_cdesc_t *) &that, NULL, CFI_attribute_other,
		CFI_type_float, 0, 1, NULL);

  *status = CFI_section((CFI_cdesc_t *) &that, this, lb, ub, st);

  if (*status != CFI_SUCCESS)
    {
      printf("FAIL C: status is %i\n",status);
      return;
    }

  value = CFI_is_contiguous((CFI_cdesc_t *) &that);
  err = ((flag == 0 && value != 0)
	 || (flag == 1 && value != 1)
	 || (flag == 2 && value != 0));

  if (err)
    {
      printf("FAIL C: contiguity for flag value %i - is %i\n", flag, value);
      *status = 10;
      return;
    }

  sum = 0;
  for (int i = 0; i < that.dim[0].extent; i++)
    {
      CFI_index_t idx[] = {i};
      sum += (int)(*(float *)CFI_address ((CFI_cdesc_t *)&that, idx));
    }

  if (sum != chksum[flag])
    {
      printf ("FAIL C: check sum = %d(%d)\n", sum, chksum[flag]);
      *status = 11;
      return;
    }

    sa((CFI_cdesc_t *) &that, flag, status);
}
