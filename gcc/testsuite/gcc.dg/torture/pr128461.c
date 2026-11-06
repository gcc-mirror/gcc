/* { dg-do run } */

#pragma scalar_storage_order big-endian
struct S {
  short a;
};
#pragma scalar_storage_order default

struct S g[2];
int n = 1;

int main ()
{
  int x = 0;
  g[1].a = 2;
  for (int i = 0; i < n; i++)
    {
      x = g[1].a;
      (&g[0])[1].a = 0;
    }
  if (x != 2)
    __builtin_abort ();
  return 0;
}
