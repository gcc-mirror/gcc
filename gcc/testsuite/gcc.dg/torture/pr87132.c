/* { dg-do run } */

extern void abort (void);
int c, d;
int main()
{
  int e[] = {4, 4, 4, 4, 4, 4, 4, 4, 4};
  d = 8;
  for (; d; d--)
    for (int a = 0; a <= 8; a++)
      {
	c = e[1];
	e[d] = 0;
      }
  if (c != 0)
    abort ();
  return 0;
}
