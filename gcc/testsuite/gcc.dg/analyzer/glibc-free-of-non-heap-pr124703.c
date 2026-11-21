/* Regression test for -Wanalyzer-free-of-non-heap false positive
   (PR analyzer/124703) reduced from glibc's dynarray implementation
   (malloc/dynarray-skeleton.c).  */
   
/* { dg-additional-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

#define SCRATCH_SIZE 4

struct dynarray {
  size_t used;
  char *array;
  char scratch[SCRATCH_SIZE];
};

static void
dynarray_init (struct dynarray *list)
{
  list->used = 0;
  list->array = list->scratch;
}

static inline void
dynarray_free_array (struct dynarray *list)
{
  if (list->array != list->scratch)
    __builtin_free (list->array); /* { dg-bogus "-Wanalyzer-free-of-non-heap" "PR analyzer/124703" } */
}

static void
dynarray_free (struct dynarray *list)
{
  dynarray_free_array (list);
  dynarray_init (list);
}

static int
dynarray_add (struct dynarray *list, char c)
{
  /* Don't add past the scratch space.  But this never happens in this test.  */
  if (list->used == SCRATCH_SIZE)
    return 1;

  /* Add element when scratch space is still available.  */
  list->array[list->used++] = c;
  return 0;
}

struct parser {
  struct dynarray items;
};

int
main (void)
{
  struct parser p;
  dynarray_init (&p.items);

  /* All these adds only go into the scratch space.  */
  dynarray_add (&p.items, 'a');
  dynarray_add (&p.items, 'b');
  dynarray_add (&p.items, 'c');
  dynarray_add (&p.items, 'd');

  dynarray_free (&p.items);
  return 0;
}
