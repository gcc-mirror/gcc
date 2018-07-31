typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern int inside_main;

__attribute__ ((__noinline__))
size_t
strnlen (const char *s, size_t n)
{
  size_t i;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  i = 0;
  while (s[i] != 0 && n--)
    i++;

  return i;
}
