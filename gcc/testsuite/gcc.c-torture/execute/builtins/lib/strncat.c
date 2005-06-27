extern void abort(void);
extern int inside_main;

typedef __SIZE_TYPE__ size_t;

char *
strncat (char *s1, const char *s2, size_t n)
{
  char *dest = s1;
  char c = '\0';
#ifdef __OPTIMIZE__
  if (inside_main)
    abort();
#endif
  while (*s1) s1++;
  c = '\0';
  while (n > 0)
    {
      c = *s2++;
      *s1++ = c;
      if (c == '\0')
	return dest;
      n--;
    }
  if (c != '\0')
    *s1 = '\0';
  return dest;
}
