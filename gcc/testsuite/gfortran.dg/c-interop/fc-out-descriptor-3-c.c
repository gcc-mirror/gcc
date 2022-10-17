#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest1 (int iinit, int jinit, CFI_cdesc_t *p);
extern void ctest2 (int iinit, int jinit, CFI_cdesc_t *a);

struct m {
  int i;
  int j;
};

void
ctest1 (int iinit, int jinit, CFI_cdesc_t *p)
{
  struct m *mp;
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (p);

  if (p->rank != 0)
    abort ();
  if (p->attribute != CFI_attribute_pointer)
    abort ();
  if (p->type != CFI_type_struct)
    abort ();

  check_CFI_status ("CFI_allocate",
		    CFI_allocate (p, NULL, NULL, sizeof (struct m)));

  if (p->base_addr == NULL)
    abort ();

  mp = (struct m *) CFI_address (p, NULL);
  mp->i = iinit;
  mp->j = jinit;
}


void
ctest2 (int iinit, int jinit, CFI_cdesc_t *a)
{
  struct m *mp;
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (a->rank != 0)
    abort ();
  if (a->attribute != CFI_attribute_allocatable)
    abort ();
  if (a->type != CFI_type_struct)
    abort ();

  /* The intent(out) allocatable array is supposed to be deallocated
     automatically on entry, if it was previously allocated.  */
  if (a->base_addr)
    abort ();  

  check_CFI_status ("CFI_allocate",
		    CFI_allocate (a, NULL, NULL, sizeof (struct m)));

  if (a->base_addr == NULL)
    abort ();

  mp = (struct m *) CFI_address (a, NULL);
  mp->i = iinit;
  mp->j = jinit;
}
