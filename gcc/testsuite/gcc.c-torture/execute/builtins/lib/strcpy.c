extern int inside_main;

char *
strcpy (char *d, const char *s)
{
  char *r = d;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
  while ((*d++ = *s++));
  return r;
}
