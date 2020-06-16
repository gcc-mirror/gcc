/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-scev-cprop" } */

char b (void);
char *d;
int e;
int f;
void
g (char *h)
{
  while (d)
    {
      long i = b ();
      if (h + i > d)
	break;
      if (f > 0 || e)
	do
	  *h++ = *h;
	while (--i);
    }
}
