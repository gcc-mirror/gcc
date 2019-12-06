/* PR middle-end/91195 */
/* { dg-do compile } */
/* { dg-options "-Wmaybe-uninitialized -O2" } */

int bar (char*);

void
foo (char *x, char *y)
{
  char *a[2];
  int b = 0;

  if (x)
    a[b++] = x;		/* { dg-bogus "may be used uninitialized in this function" } */
  if (y)
    a[b++] = y;

  for (int j = 0; j < 4; j++) 
    switch (j)
      {
      case 0:
	if (b == 0 || bar (a[0]))
	  break;
      }
}
