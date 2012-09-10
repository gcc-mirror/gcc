/* { dg-do compile } */

char *a;
void
fn1 ()
{
  char *p = a;
  while (p && *p != '\0')
    {
      while (*p == '\t')
	*p++ = '\0';
      if (*p != '\0')
	p = 0;
    }
}
