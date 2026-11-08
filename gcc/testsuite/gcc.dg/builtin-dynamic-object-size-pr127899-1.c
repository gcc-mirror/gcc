/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

#define container_of(ptr, type, member) \
  ((type *) ((char *) (ptr) - __builtin_offsetof (type, member)))

struct S
{
  int count;
  char data[] __attribute__ ((counted_by (count)));
};

__attribute__ ((noinline)) size_t
min_size (struct S *s)
{
  /* Back from the flexible array member to the structure holding it.  */
  void *data = s->data;
  struct S *again = container_of (data, struct S, data);

  return __builtin_dynamic_object_size (again, 2);
}

int
main (void)
{
  struct S *s = __builtin_malloc (sizeof (*s) + 10);

  if (!s)
    return 1;
  s->count = 10;
  __builtin_printf ("%zu\n", min_size (s));
  __builtin_free (s);
  return 0;
}
