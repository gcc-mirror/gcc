#include <ISO_Fortran_binding.h>
#include <stdio.h>
#include <stdlib.h>

/* Part of the test for the fix of PR88929 - see ISO_Fortran_binding_3.f90. */

int c_test (CFI_cdesc_t * a_desc)
{
  CFI_index_t idx[2];
  int *res_addr;
  int err = 1; /* this error code represents all errors */

  if (a_desc->rank != 2)
    return err;

  if (a_desc->type != CFI_type_int)
    return err;

  err = 0;
  for (idx[0] = 0; idx[0] < a_desc->dim[0].extent; idx[0]++)
    for (idx[1] = 0; idx[1] < a_desc->dim[1].extent; idx[1]++)
      {
	res_addr = CFI_address (a_desc, idx);
	err += *res_addr;
	*res_addr = *res_addr + 1;
      }

  if (err != 10) return 1;

  return 0;
}

