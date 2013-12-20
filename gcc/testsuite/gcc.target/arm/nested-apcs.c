/* { dg-do run } */
/* { dg-options "-fno-omit-frame-pointer -mapcs-frame -O" } */

extern void abort (void);

struct x
{
  int y;
  int z;
};

int __attribute__((noinline)) f (int c, int d, int e, int h, int i)
{
  int a;
  struct x b;

  int __attribute__((noinline)) g (int p, int q, int r, struct x s)
  {
    return a + p + q + r + s.y + s.z;
  }

  a = 5;
  b.y = h;
  b.z = i;

  return g(c, d, e, b);
}

int main(void)
{
  if (f (1, 2, 3, 4, 5) != 20)
    abort();
  return 0;
}
