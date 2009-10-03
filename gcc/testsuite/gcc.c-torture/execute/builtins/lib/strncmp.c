extern void abort (void);
extern int inside_main;

typedef __SIZE_TYPE__ size_t;

__attribute__ ((__noinline__))
int
strncmp(const char *s1, const char *s2, size_t n)
{
  const unsigned char *u1 = (const unsigned char *)s1;
  const unsigned char *u2 = (const unsigned char *)s2;
  unsigned char c1, c2;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif

  while (n > 0)
    {
      c1 = *u1++, c2 = *u2++;
      if (c1 == '\0' || c1 != c2)
	return c1 - c2;
      n--;
    }
  return c1 - c2;
}
