#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

struct t {
  float xyz[3];
  int id;
};

extern void testit_f_bind_c (CFI_cdesc_t *a, float x, float y, float z);
extern void testit_c (CFI_cdesc_t *a, float x, float y, float z);

void testit_c (CFI_cdesc_t *a, float x, float y, float z)
{
  struct t *tp;

  /* Check that the allocatable dummy is unallocated on entry and do
     some other sanity checks.  */
  dump_CFI_cdesc_t (a);
  if (a->attribute != CFI_attribute_allocatable)
    abort ();
  if (a->rank)
    abort ();
  if (a->base_addr)
    abort ();

  /* Allocate and initialize the output argument.  */
  CFI_allocate (a, NULL, NULL, 0);
  if (!a->base_addr)
    abort ();
  tp = (struct t *) CFI_address (a, NULL);
  tp->id = 42;
  tp->xyz[0] = 0.0;
  tp->xyz[1] = 0.0;
  tp->xyz[2] = 0.0;

  /* Now call the Fortran function, which is supposed to automatically
     deallocate the object we just created above and point the descriptor
     at a different object.  */
  testit_f_bind_c (a, x, y, z);

  /* Make sure we've got an allocated object, initialized as we
     expect.  */
  if (!a->base_addr)
    abort ();
  tp = (struct t *) CFI_address (a, NULL);
  if (tp->id != -1)
    abort ();
  if (tp->xyz[0] != x || tp->xyz[1] != y || tp->xyz[2] != z)
    abort ();
}
