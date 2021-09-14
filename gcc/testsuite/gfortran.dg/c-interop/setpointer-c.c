#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

static int a[10][5][3];
static CFI_index_t extents[] = {3, 5, 10};
static CFI_index_t lb1[] = {1, 2, 3};
static CFI_index_t lb2[] = {0, 1, -10};

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  CFI_CDESC_T(3) sdesc;
  CFI_cdesc_t *source = (CFI_cdesc_t *) &sdesc;
  CFI_CDESC_T(3) rdesc;
  CFI_cdesc_t *result = (CFI_cdesc_t *) &rdesc;

  /* Create descriptors.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)a, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, extents));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));

  /* Use setpointer to adjust the bounds of source in place.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (source, source, lb1));
  dump_CFI_cdesc_t (source);
  if (source->dim[0].lower_bound != lb1[0])
    abort ();
  if (source->dim[1].lower_bound != lb1[1])
    abort ();
  if (source->dim[2].lower_bound != lb1[2])
    abort ();

  /* Use setpointer to copy the pointer and bounds from source.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (result, source, NULL));
  dump_CFI_cdesc_t (result);
  if (result->base_addr != source->base_addr)
    abort ();
  if (result->dim[0].lower_bound != source->dim[0].lower_bound)
    abort ();
  if (result->dim[1].lower_bound != source->dim[1].lower_bound)
    abort ();
  if (result->dim[2].lower_bound != source->dim[2].lower_bound)
    abort ();

  /* Use setpointer to nullify result.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (result, NULL, NULL));
  dump_CFI_cdesc_t (result);
  if (result->base_addr)
    abort ();

  /* Use setpointer to copy the pointer from source, but use
     different bounds.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (result, source, lb2));
  dump_CFI_cdesc_t (source);
  if (result->base_addr != source->base_addr)
    abort ();
  if (result->dim[0].lower_bound != lb2[0])
    abort ();
  if (result->dim[1].lower_bound != lb2[1])
    abort ();
  if (result->dim[2].lower_bound != lb2[2])
    abort ();
}

