extern void abort (void);
extern int inside_main;

int
strcmp (const char *s1, const char *s2)
{
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  while (*s1 != 0 && *s1 == *s2)
    s1++, s2++;

  if (*s1 == 0 || *s2 == 0)
    return (unsigned char) *s1 - (unsigned char) *s2;
  return *s1 - *s2;
}
