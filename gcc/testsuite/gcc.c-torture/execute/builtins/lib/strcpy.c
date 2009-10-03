extern void abort (void);
extern int inside_main;

__attribute__ ((__noinline__))
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
