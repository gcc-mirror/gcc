typedef long int idx_t;
extern void rpl_free (void *ptr);

void *xicalloc (idx_t n, idx_t s)
  __attribute__ ((__malloc__))
  __attribute__ ((__malloc__ (rpl_free, 1)))
  __attribute__ ((__alloc_size__ (1, 2)))
  __attribute__ ((__returns_nonnull__));

void *
xizalloc (idx_t s)
{
  return xicalloc (s, 1);
}
