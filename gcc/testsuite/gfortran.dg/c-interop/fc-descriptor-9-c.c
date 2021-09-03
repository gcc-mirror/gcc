/* 8.3.1: ISO_Fortran_binding.h may be included more than once.  */

#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);

struct descriptor_fixed_part {
  void *base_addr;
  size_t elem_len;
  int version;
};

void
ctest (CFI_cdesc_t *a)
{
  struct descriptor_fixed_part *f = (struct descriptor_fixed_part *) a;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* The first three members of the structure shall be base_addr,
     elem_len, and version in that order.  */
  if (&(a->base_addr) != &(f->base_addr))
    abort ();
  if (&(a->elem_len) != &(f->elem_len))
    abort ();
  if (&(a->version) != &(f->version))
    abort ();

  /* The final member shall be dim, with the other members after version
     and before dim in any order.  */
  if ((void *)&(a->rank) >= (void *)a->dim)
    abort ();
  if ((void *)&(a->type) >= (void *)a->dim)
    abort ();
  if ((void *)&(a->attribute) >= (void *)a->dim)
    abort ();
}
