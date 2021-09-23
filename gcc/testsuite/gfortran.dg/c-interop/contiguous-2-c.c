#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest1 (CFI_cdesc_t *a);
extern void ctest2 (CFI_cdesc_t *a);
extern void ftest1 (CFI_cdesc_t *a, int first, int last, int step);
extern void ftest2 (CFI_cdesc_t *a, int first, int last, int step);

#if 0
static void
dump_array (CFI_cdesc_t *a, const char *name, const char *note)
{
  int i;

  fprintf (stderr, "%s\n", note);
  for (i = 0; i < a->dim[0].extent; i++)
    {
      int j = i + a->dim[0].lower_bound;
      int elt;
      CFI_index_t sub[1];
      sub[0] = j;
      elt = *((int *) CFI_address (a, sub));
      fprintf (stderr, "%s[%d] = %d\n", name, j, elt);
    }
  fprintf (stderr, "\n");
}
#else
#define dump_array(a, name, note) 
#endif

static void
ctest (CFI_cdesc_t *a, int lb, int ub, int s,
       void (*fn) (CFI_cdesc_t *, int, int, int))
{
  CFI_CDESC_T(1) bdesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
  CFI_index_t lb_array[1], ub_array[1], s_array[1];
  int i;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* Make sure we got a valid descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(int))
    abort ();
  if (a->rank != 1)
    abort ();
  if (a->type != CFI_type_int)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();

  /* Create an array section and pass it to fn.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (b, NULL, CFI_attribute_other,
				   CFI_type_int,
				   sizeof (int), 1, NULL));
  lb_array[0] = lb - 1 + a->dim[0].lower_bound;
  ub_array[0] = ub - 1 + a->dim[0].lower_bound;
  s_array[0] = s;
  check_CFI_status ("CFI_section",
		    CFI_section (b, a, lb_array, ub_array, s_array));
  dump_CFI_cdesc_t (b);
  dump_array (b, "b", "b after CFI_section");

  /* Pass it to the Fortran function fn.  */
  if (CFI_is_contiguous (b))
    abort ();
  (*fn) (b, lb, ub, s);
  dump_CFI_cdesc_t (b);
  dump_array (b, "b", "b after calling Fortran fn");

  /* fn is supposed to negate the elements of the array section it
     receives.  Check that the original array has been updated.  */
  dump_array (a, "a", "a after calling Fortran fn");
  for (i = 0; i < a->dim[0].extent; i++)
    {
      int elt;
      int j = i + a->dim[0].lower_bound;
      CFI_index_t sub[1];
      sub[0] = j;
      elt = *((int *) CFI_address (a, sub));
      if (i + 1 >= lb && i + 1 <= ub && (i + 1 - lb) % s == 0)
	{
          if (elt != - (i + 1))
	    abort ();
	}
      else if (elt != (i + 1))
	abort ();
    }
}


/* Entry points for the Fortran side.  */

void
ctest1 (CFI_cdesc_t *a)
{
  ctest (a, 5, 13, 2, ftest1);
}

void
ctest2 (CFI_cdesc_t *a)
{
  ctest (a, 8, 20, 3, ftest2);
}

