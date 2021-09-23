#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest1 (CFI_cdesc_t *a, int first, int last, int step);
extern void ctest2 (CFI_cdesc_t *a, int first, int last, int step);
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
ctest (CFI_cdesc_t *a, int first, int last, int step,
       void (*fn) (CFI_cdesc_t *, int, int, int))
{
  int i;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);
  dump_array (a, "a", "a on input to ctest");

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

  /* Pass it to the Fortran function fn.  */
  (*fn) (a, first, last, step);
  dump_CFI_cdesc_t (a);
  dump_array (a, "a", "a after calling Fortran fn");
}

/* Entry points for the Fortran side.
   Note that the Fortran code has already created the array section
   and these functions were declared without the CONTIGUOUS attribute
   so they receive a non-contiguous array.  The magic is supposed to
   happen when we pass them back into a Fortran function declared with
   the CONTIGUOUS attribute.  */

void
ctest1 (CFI_cdesc_t *a, int first, int last, int step)
{
  ctest (a, first, last, step, ftest1);
}

void
ctest2 (CFI_cdesc_t *a, int first, int last, int step)
{
  ctest (a, first, last, step, ftest2);
}
