extern int inside_main;

char *
strcpy (char *d, const char *s)
{
  char *r = d;
#if defined __OPTIMIZE__ && !defined __OPTIMIZE_SIZE__
  if (inside_main)
    abort ();
#endif
  while ((*d++ = *s++));
  return r;
}
