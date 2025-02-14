/* { dg-add-options vect_early_break } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-additional-options "-fallow-store-data-races -fprofile-arcs" } */
/* { dg-additional-options "-msse4.2 -mprefer-vector-width=128" { target { x86_64-*-* i?86-*-* } } } */

#include "tree-vect.h"

typedef unsigned int hashval_t;
struct htab {
  void ** entries;
  unsigned long size;
};
typedef struct htab *htab_t;
unsigned long htab_size (htab_t h)
{
  return h->size;
}
typedef struct
{
  htab_t htab;
  void * *slot;
  void * *limit;
} htab_iterator;

static inline void *
first_htab_element (htab_iterator *hti, htab_t table)
{
  hti->htab = table;
  hti->slot = table->entries;
  hti->limit = hti->slot + htab_size (table);
  do
    {
      void * x = *(hti->slot);
      if (x != ((void *) 0) && x != ((void *) 1))
 break;
    } while (++(hti->slot) < hti->limit);

  if (hti->slot < hti->limit)
    return *(hti->slot);
  return (void *) 0;
}

static inline unsigned char
end_htab_p (const htab_iterator *hti)
{
  if (hti->slot >= hti->limit)
    return 1;
  return 0;
}

static inline void *
next_htab_element (htab_iterator *hti)
{
  while (++(hti->slot) < hti->limit)
    {
      void * x = *(hti->slot);
      if (x != ((void *) 0) && x != ((void *) 1))
	return x;
    }
  return (void *) 0;
}

typedef unsigned long vn_nary_op_t;

typedef struct vn_tables_s
{
  htab_t nary;
} *vn_tables_t;

vn_tables_t valid_info;

void __attribute__((noipa))
announce (vn_nary_op_t p)
{
  static vn_nary_op_t prev = 0;
  if (prev == 0x70904f0 && p != 0x70904c0)
    __builtin_abort ();
  prev = p;
}

void __attribute__((noipa))
set_hashtable_value_ids_1 (void)
{
  htab_iterator hi;
  vn_nary_op_t vno;
  for (vno = (vn_nary_op_t) first_htab_element (&(hi), (valid_info->nary)); !end_htab_p (&(hi)); vno = (vn_nary_op_t) next_htab_element (&(hi)))
    announce (vno);
}

int main()
{
  if (sizeof (void *) != sizeof (vn_nary_op_t))
    return 0;
  check_vect ();
  valid_info = __builtin_malloc (sizeof (struct vn_tables_s));
  valid_info->nary = __builtin_malloc (sizeof (struct htab));
  valid_info->nary->entries = __builtin_malloc (sizeof (void *) * 32);
  valid_info->nary->size = 31;
  static vn_nary_op_t x[] = { 0x70905e0, 0x0, 0x0, 0x7090610, 0x7090550, 0x7090400, 0x70903a0, 0x0, 
      0x0, 0x70904f0, 0x0, 0x0, 0x0, 0x0, 0x70904c0, 0x7090520, 0x7090460, 
      0x7090490, 0x7090430, 0x0, 0x0, 0x0, 0x7090640, 0x0, 0x0, 0x70903d0, 0x0, 
      0x7090580, 0x0, 0x0, 0x70905b0};
  __builtin_memcpy (valid_info->nary->entries, x, sizeof (void *) * 31);
  set_hashtable_value_ids_1 ();
}
