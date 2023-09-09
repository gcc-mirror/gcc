typedef signed long idx_t;

void *xirealloc (void *p, idx_t s)
  __attribute__ ((__alloc_size__ (2))) __attribute__ ((__returns_nonnull__));

char *
test_cast_1 (char *buf, idx_t buf_count)
{
  return (char *) xirealloc (buf, buf_count + 1);
}

void *alloc_cast_2 (signed char x, signed char y)
  __attribute__ ((__alloc_size__ (1, 2)));

void *
test_cast_2 (signed char a, signed char b)
{
  return alloc_cast_2 (a + 1, b + 1);
}
