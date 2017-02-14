/* { dg-do compile } */

int a;
long b;
long c;
void d()
{
  int e = 0;
  for (; b; b++)
    if (c)
      {
	e++;
	e++;
      }
  while (e)
    a = e -= 2;
}
