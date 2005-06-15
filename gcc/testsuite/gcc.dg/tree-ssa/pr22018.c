/* { dg-do run }  */
/* { dg-options -O2 }  */

void abort (void);
void g(int);
void f(int l)
{
  unsigned i;
  for (i = 0; i < l; i++)
    {
      int y = i;
      /* VRP was wrongfully computing z's range to be [0, 0] instead
	 of [-INF, 0].  */
      int z = y*-32;
      g(z);
    }
}

void g(int i)
{
  static int x = 0;
  if (i == 0)
    x ++;
  if (x > 1)
    abort ();
}

int main(void)
{
  f(3);
  return 0;
}
