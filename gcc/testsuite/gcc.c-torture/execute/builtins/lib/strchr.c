extern int inside_main;

char *
strchr (const char *s, int c)
{
#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  for (;;)
    {
      if (*s == c)
	return (char *) s;
      if (*s == 0)
	return 0;
      s++;
    }
}

char *
index (const char *s, int c)
{
  return strchr (s, c);
}
