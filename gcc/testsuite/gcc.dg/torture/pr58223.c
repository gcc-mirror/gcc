/* { dg-do run } */

extern void abort (void);
int a[2], b;

int main ()
{
  for (b = 0; b < 2; b++)
    {
      a[0] = 1;
      a[b] = 0;
    }
  if (a[0] != 1)
    abort ();
  return 0;
}
