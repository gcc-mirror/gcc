/* { dg-do run } */
/* { dg-options "-O1 -fno-delete-null-pointer-checks" } */
extern void abort (void);
int g = 0;
static int __attribute__((noinline)) f (long long a, long long b)
{
  int cmp;
  cmp = a > b;
  if (&g == 0)
    cmp-=2;
  else
    cmp++;
  return cmp;
}

int main (void)
{
  int ret = f (2, 1);
  if (ret != 2)
    abort ();
  return 0;
}
