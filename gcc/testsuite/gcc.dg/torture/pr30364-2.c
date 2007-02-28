/* { dg-do run } */

extern void abort (void);

int f(unsigned int a, unsigned int b)
{
  if (a > 0x7FFFFFF0) return 0;
  if (b > 0x7FFFFFF0) return 0;

  int c = (a - 20) + (b - 20);
  return c > 0x7FFFFFF0;
}

int main()
{
  if (f (0x7FFFFFF0, 41) != 1)
    abort ();
  return 0;
}
