extern int inside_main;
extern void abort(void);

__attribute__ ((__noinline__))
char *
strcat (char *dst, const char *src)
{
  char *p = dst;
  
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  while (*p)
    p++;
  while ((*p++ = *src++))
    ;
  return dst;
}
