extern void abort (void);
extern int inside_main;

__SIZE_TYPE__
strlen (const char *s)
{
  __SIZE_TYPE__ i;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  i = 0;
  while (s[i] != 0)
    i++;

  return i;
}
