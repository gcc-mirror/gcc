#include <ISO_Fortran_binding.h>

void
test_c (CFI_cdesc_t *x, size_t n, int num)
{
  if (!x->base_addr)
    __builtin_abort ();
  if (x->version != CFI_VERSION)
    __builtin_abort ();
  if (x->rank != 1)
    __builtin_abort ();
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  if (x->dim[0].lower_bound != 0)
    __builtin_abort ();
  if (x->dim[0].extent != 3)
    __builtin_abort ();

  if (x->elem_len != n || x->dim[0].sm != n)
    __builtin_abort ();

  if (num == 1 && x->type != CFI_type_int16_t)
    __builtin_abort ();
  if (num == 2 && x->type != CFI_type_double_Complex)
    __builtin_abort ();
}
