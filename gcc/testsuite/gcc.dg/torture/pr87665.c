/* { dg-do run } */

struct X { long x; long y; };

struct X a[1024], b[1024];

void foo ()
{
  for (int i = 0; i < 1024; ++i)
    {
      long tem = a[i].x;
      a[i].x = 0;
      b[i].x = tem;
      b[i].y = a[i].y;
    }
}

int main()
{
  for (int i = 0; i < 1024; ++i)
    a[i].x = i;
  foo ();
  for (int i = 0; i < 1024; ++i)
    if (b[i].x != i)
      __builtin_abort();
  return 0;
}
