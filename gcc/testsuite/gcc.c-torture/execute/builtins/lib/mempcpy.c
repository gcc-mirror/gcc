extern void abort (void);
extern int inside_main;

void *
mempcpy (void *dst, const void *src, __SIZE_TYPE__ n)
{
  const char *srcp;
  char *dstp;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  srcp = src;
  dstp = dst;
  while (n-- != 0)
    *dstp++ = *srcp++;

  return dstp;
}
