/*  PR tree-optimization/12899  */

void
bb_getopt_ulflags (char *s)
{
  for (;;)
    if (s[1])
      do
	s++;
      while (*s);
}
