/* { dg-options "-fira-algorithm=priority" } */
char **
foo (char **p, char *cmp, unsigned i)
{
  for (; *p; p++)
    if (__builtin_strncmp (*p, cmp, i))
      if (i == __builtin_strlen (*p))
	break;
  return p;
}
