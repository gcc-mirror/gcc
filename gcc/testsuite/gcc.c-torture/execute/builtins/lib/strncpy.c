extern void abort(void);
extern int inside_main;

typedef __SIZE_TYPE__ size_t;

char *
strncpy(char *s1, const char *s2, size_t n)
{
  char *dest = s1;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif
  for (; *s2 && n; n--)
    *s1++ = *s2++;
  while (n--)
    *s1++ = 0;
  return dest;
}

