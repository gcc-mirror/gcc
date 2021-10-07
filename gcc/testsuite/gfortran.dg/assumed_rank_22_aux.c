/* Called by assumed_rank_22.f90.  */

#include <ISO_Fortran_binding.h>
#include <assert.h>

void
c_assumed (CFI_cdesc_t *x, int num)
{
  assert (num == 0 || num == 20 || num == 40 || num == 60 || num == 80
	  || num == 100);
  assert (x->elem_len == sizeof (int));
  assert (x->rank == 3);
  assert (x->type == CFI_type_int32_t);

  assert (x->attribute == CFI_attribute_other);
  assert (x->dim[0].lower_bound == 0);
  assert (x->dim[1].lower_bound == 0);
  assert (x->dim[2].lower_bound == 0);
  assert (x->dim[0].extent == 5);
  assert (x->dim[1].extent == 4);
  if (num == 0)
    assert (x->dim[2].extent == -1);
  else if (num == 20)
    assert (x->dim[2].extent == 1);
  else if (num == 40)
    {
      /* FIXME: - dg-output = 'c_assumed ... OK' checked in .f90 file. */
      /* assert (x->dim[2].extent == 0); */
      if (x->dim[2].extent == 0)
	__builtin_printf ("c_assumed - 40 - OK\n");
      else
	__builtin_printf ("error: c_assumed num=%d: "
		      "x->dim[2].extent = %d != 0\n",
		      num, x->dim[2].extent);
    }
  else if (num == 60)
    assert (x->dim[2].extent == 2);
  else if (num == 80)
    assert (x->dim[2].extent == 2);
  else if (num == 100)
    {
      /* FIXME: - dg-output = 'c_assumed ... OK' checked in .f90 file. */
      /* assert (x->dim[2].extent == 0); */
      if (x->dim[2].extent == 0)
	__builtin_printf ("c_assumed - 100 - OK\n");
      else
	__builtin_printf ("error: c_assumed num=%d: "
		      "x->dim[2].extent = %d != 0\n",
		      num, x->dim[2].extent);
    }
  else
    assert (0);
}

void
c_allocated (CFI_cdesc_t *x)
{
  assert (x->elem_len == sizeof (int));
  assert (x->rank == 3);
  assert (x->type == CFI_type_int32_t);
  assert (x->attribute == CFI_attribute_allocatable);
  assert (x->dim[0].lower_bound == -1);
  assert (x->dim[1].lower_bound == 1);
  assert (x->dim[2].lower_bound == -1);
  assert (x->dim[0].extent == 5);
  assert (x->dim[1].extent == 4);
  assert (x->dim[2].extent == 1);
}
