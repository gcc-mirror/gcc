extern int inside_main;

void *
memset (void *dst, int c, __SIZE_TYPE__ n)
{
  /* Single-byte memsets should be done inline when optimisation
     is enabled.  */
#ifdef __OPTIMIZE__
  if (inside_main && n < 2)
    abort ();
#endif

  while (n-- != 0)
    n[(char *) dst] = c;

  return dst;
}
