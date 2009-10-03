extern int inside_main;

__attribute__ ((__noinline__))
void
bfill (void *s, __SIZE_TYPE__ n, int ch)
{
  char *p;

  for (p = s; n-- > 0; p++)
    *p = ch;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
}
