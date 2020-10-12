/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve" } */

void g(void);

int a[8][3];
int b;
void c(void)
{
  int d[] = {7, 3};
  int *e = a[0];
  int f;
  b = 0;
  for (; b < 8; b++)
    {
      f = 0;
      for (; f < 3; f++)
	a[b][f] = 0;
    }
  g();
  *e = (long)d;
}
