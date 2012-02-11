void bad (void);
char *foo (char *src, char **last)
{
  char *dst;
  int ch;
  dst = src = (src ? src : *last);

  if (*src == 0)
    return 0;

  while (src[0])
    {
      if (!src[1])
	{
	  bad ();
	  break;
	}
      *dst = *src;
      dst += 1;
      src += 2;
    }
  *last = src;
  *dst = 0;
  return *last;
}
