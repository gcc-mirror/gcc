extern int inside_main;

char *
strrchr (const char *s, int c)
{
  __SIZE_TYPE__ i;

#ifdef __OPTIMIZE__
  if (inside_main)
    abort ();
#endif

  i = 0;
  while (s[i] != 0)
    i++;

  do
    if (s[i] == c)
      return (char *) s + i;
  while (i-- != 0);

  return 0;
}

char *
rindex (const char *s, int c)
{
  return strrchr (s, c);
}
