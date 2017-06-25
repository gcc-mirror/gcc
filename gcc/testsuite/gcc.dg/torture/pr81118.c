/* { dg-do compile } */
/* { dg-additional-options "-w" } */

int a[7], b;
int c()
{
  int d, e;
  for (; d; d++)
    if (a[d])
      if (b)
	return;
      else if (d >= e)
	return 0;
}
