extern int inside_main;

char *
strstr(const char *s1, const char *s2)
{
  const char *p, *q;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  /* deliberately dumb algorithm */
  for (; *s1; s1++)
    {
      p = s1, q = s2;
      while (*q && *p)
	{
	  if (*q != *p)
	    break;
	  p++, q++;
	}
      if (*q == 0)
	return (char *)s1;
    }
  return 0;
}
