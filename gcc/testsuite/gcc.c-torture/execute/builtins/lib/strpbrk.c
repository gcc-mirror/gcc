extern void abort (void);
extern int inside_main;

__attribute__ ((__noinline__))
char *
strpbrk(const char *s1, const char *s2)
{
  const char *p;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
  while (*s1)
    {
      for (p = s2; *p; p++)
	if (*s1 == *p)
	  return (char *)s1;
      s1++;
    }
  return 0;
}
