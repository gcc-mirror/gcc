extern void abort(void);
extern int inside_main;

__attribute__ ((__noinline__))
void *
memchr (const void *s, int c, __SIZE_TYPE__ n)
{
  const unsigned char uc = c;
  const unsigned char *sp;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  sp = s;
  for (; n != 0; ++sp, --n)
    if (*sp == uc)
      return (void *) sp;

  return 0;
}
