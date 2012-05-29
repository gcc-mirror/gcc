/* { dg-do run } */
/* { dg-options "-ftree-vectorize -ftree-loop-distribute-patterns" } */

extern void abort (void);

struct Foo
{
  char a : 1;
  char b : 7;
};

struct Foo x[256];
int y[256];

void __attribute__((noinline,noclone)) bar (int n)
{
  int i;
  for (i = 0; i < n; ++i)
    {
      x[i].a = 0;
      y[i] = 3;
    }
}

int main()
{
  x[5].b = 7;
  bar (256);
  if (x[5].b != 7)
    abort ();
  return 0;
}
