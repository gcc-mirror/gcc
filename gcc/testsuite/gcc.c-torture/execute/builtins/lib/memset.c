extern void abort (void);
extern int inside_main;

void *
memset (void *dst, int c, __SIZE_TYPE__ n)
{
  while (n-- != 0)
    n[(char *) dst] = c;

  /* Single-byte memsets should be done inline when optimisation
     is enabled.  Do this after the copy in case we're being called to
     initialize bss.  */
#ifdef __OPTIMIZE__
  if (inside_main && n < 2)
    abort ();
#endif

  return dst;
}
