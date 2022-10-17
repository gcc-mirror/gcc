#include <ISO_Fortran_binding.h>

intptr_t assumed_rank_alloc_f (CFI_cdesc_t *);
intptr_t assumed_rank_pointer_f (CFI_cdesc_t *);
intptr_t assumed_rank_f (CFI_cdesc_t *);
intptr_t assumed_rank_cont_f (CFI_cdesc_t *);
intptr_t assumed_shape_f (CFI_cdesc_t *);
intptr_t assumed_shape_cont_f (CFI_cdesc_t *);
intptr_t deferred_shape_alloc_f (CFI_cdesc_t *);
intptr_t deferred_shape_pointer_f (CFI_cdesc_t *);


static void
basic_check(CFI_cdesc_t *x)
{
  if (!x->base_addr)
    __builtin_abort ();
  if (x->elem_len != sizeof(int32_t))
    __builtin_abort ();
  if (x->version != CFI_VERSION)
    __builtin_abort ();
  if (x->rank != 4)
    __builtin_abort ();
  if (x->type != CFI_type_int32_t)
    __builtin_abort ();
  if (x->attribute == CFI_attribute_other)
    {
      if (x->dim[0].lower_bound != 0)
	__builtin_abort ();
      if (x->dim[1].lower_bound != 0)
	__builtin_abort ();
      if (x->dim[2].lower_bound != 0)
	__builtin_abort ();
      if (x->dim[3].lower_bound != 0)
	__builtin_abort ();
    }
}

intptr_t
assumed_rank_alloc_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (!CFI_is_contiguous (x))
    __builtin_abort ();
  if (x->attribute != CFI_attribute_allocatable)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = assumed_rank_alloc_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}

intptr_t
assumed_rank_pointer_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (x->attribute != CFI_attribute_pointer)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = assumed_rank_pointer_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}


intptr_t
assumed_rank_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = assumed_rank_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}

intptr_t
assumed_rank_cont_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (!CFI_is_contiguous (x))
    __builtin_abort ();
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = assumed_rank_cont_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}

intptr_t
assumed_shape_c (CFI_cdesc_t *x, int num)
{
  basic_check (x);
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2;
  if (num == 1 || num == 2 || num == 3)
    {
      if (!CFI_is_contiguous (x))
	__builtin_abort ();
    }
  else
    {
      if (CFI_is_contiguous (x))
	__builtin_abort ();
    }

  if (num == 1 || num == 4)
    addr2 = assumed_shape_f (x);
  else if (num == 2 || num == 5)
    addr2 = assumed_shape_cont_f (x);
  else if (num == 3 || num == 6)
    addr2 = assumed_rank_cont_f (x);
  else
    __builtin_abort ();

  if (num == 1 || num == 2 || num == 3)
    {
      if (addr != addr2)
	__builtin_abort ();
    }
  else
    {
      if (CFI_is_contiguous (x))
	__builtin_abort ();
    }
  if (addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr2;
}

intptr_t
assumed_shape_cont_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (!CFI_is_contiguous (x))
    __builtin_abort ();
  if (x->attribute != CFI_attribute_other)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = assumed_shape_cont_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}

intptr_t
deferred_shape_alloc_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (!CFI_is_contiguous (x))
    __builtin_abort ();
  if (x->attribute != CFI_attribute_allocatable)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = deferred_shape_alloc_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}

intptr_t
deferred_shape_pointer_c (CFI_cdesc_t *x)
{
  basic_check (x);
  if (x->attribute != CFI_attribute_pointer)
    __builtin_abort ();
  intptr_t addr = (intptr_t) x->base_addr;
  intptr_t addr2 = deferred_shape_pointer_f (x);
  if (addr != addr2 || addr != (intptr_t) x->base_addr)
    __builtin_abort ();
  return addr;
}
