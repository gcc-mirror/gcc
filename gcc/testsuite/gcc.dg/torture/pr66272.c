/* { dg-do run } */

struct S
{
  int f0;
  int f1;
};

int b;

int main ()
{
  struct S a[2] = { 0 };
  struct S d = { 0, 1 };
  for (b = 0; b < 2; b++)
    {
      a[b] = d;
      d = a[0];
    }
  if (d.f1 != 1)
    __builtin_abort ();
  return 0;
}
