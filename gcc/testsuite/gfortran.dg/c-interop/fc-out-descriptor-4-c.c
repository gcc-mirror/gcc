#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest1 (int imin, int imax, int jmin, int jmax, CFI_cdesc_t *p);
extern void ctest2 (int imin, int imax, int jmin, int jmax, CFI_cdesc_t *a);

struct m {
  int i;
  int j;
};

void
ctest1 (int imin, int imax, int jmin, int jmax, CFI_cdesc_t *p)
{
  struct m *mp;
  int i, j;
  CFI_index_t lb[2], ub[2], s[2];

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (p);

  if (p->rank != 2)
    abort ();
  if (p->attribute != CFI_attribute_pointer)
    abort ();
  if (p->type != CFI_type_struct)
    abort ();

  lb[0] = imin;
  lb[1] = jmin;
  ub[0] = imax;
  ub[1] = jmax;
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (p, lb, ub, sizeof (struct m)));

  if (p->base_addr == NULL)
    abort ();

  for (j = jmin; j <= jmax; j++)
    for (i = imin; i <= imax; i++)
      {
	s[0] = i;
	s[1] = j;
	mp = (struct m *) CFI_address (p, s);
	mp->i = i;
	mp->j = j;
      }
}

void
ctest2 (int imin, int imax, int jmin, int jmax, CFI_cdesc_t *a)
{
  struct m *mp;
  int i, j;
  CFI_index_t lb[2], ub[2], s[2];

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (a->rank != 2)
    abort ();
  if (a->attribute != CFI_attribute_allocatable)
    abort ();
  if (a->type != CFI_type_struct)
    abort ();

  /* Intent(out) argument is supposed to be deallocated automatically
     on entry.  */
  if (a->base_addr)
    abort ();

  lb[0] = imin;
  lb[1] = jmin;
  ub[0] = imax;
  ub[1] = jmax;
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (a, lb, ub, sizeof (struct m)));

  if (a->base_addr == NULL)
    abort ();

  for (j = jmin; j <= jmax; j++)
    for (i = imin; i <= imax; i++)
      {
	s[0] = i;
	s[1] = j;
	mp = (struct m *) CFI_address (a, s);
	mp->i = i;
	mp->j = j;
      }
}
