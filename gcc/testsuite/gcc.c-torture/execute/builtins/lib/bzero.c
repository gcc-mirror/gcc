extern int inside_main;

__attribute__ ((__noinline__))
void
bzero (void *s, __SIZE_TYPE__ n)
{
  char *p;

  for (p = s; n-- > 0; p++)
    *p = 0;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
}
