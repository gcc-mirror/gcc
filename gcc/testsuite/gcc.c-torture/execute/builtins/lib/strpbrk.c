extern int inside_main;

char *
strpbrk(const char *s1, const char *s2)
{
  char *p;
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif
  while (*s1)
    {
      for (p = s2; *p; p++)
	if (*s1 == *p)
	  return s1;
      s1++;
    }
  return 0;
}
