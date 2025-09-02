/* PR120929, pointee's size should not be propagated to pointer for
   __builtin_dynamic_object_size.  */

/* { dg-do compile} */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

extern void pin_pointer(void *);
extern int some_value(void);

struct magic_ {
  char unused[9]; // at least 9
};

struct magic_map {
  struct magic_ *magic;
};

static int
coalesce_entries(struct magic_ **ma)
{
  size_t slen;

  slen = sizeof (**ma);
  *ma = __builtin_malloc (slen);

  for (unsigned i = 0; i < 1; i++)
    {
      char b[1024] = {};
      struct magic_ *ptr = *ma;
      (void) __builtin___memcpy_chk (ptr, b, sizeof (*ptr),  /* { dg-bogus "overflows the destination" } */
				     __builtin_dynamic_object_size (ptr, 0));
    }
  return 0;
}

struct magic_map *
apprentice_load(void)
{
  char buf[128]; // did not shrink, but needs to be more than 100
  struct magic_map *map2;

  map2 = __builtin_malloc (sizeof (*map2));

  pin_pointer(&buf);
  coalesce_entries(&map2->magic);
  pin_pointer(map2);
}
