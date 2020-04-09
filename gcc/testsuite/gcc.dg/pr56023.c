/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

void
foo (char *c)
{
  unsigned int x = 0;
  unsigned int i;

  for (i = 0; c[i]; i++)
    {
      if (i >= 5 && x != 1)
	break;
      else if (c[i] == ' ')
	x = i;
      else if (c[i] == '/' && c[i + 1] != ' ' && i)
	x = i + 1;
    }
}
