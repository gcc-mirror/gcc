/* { dg-do run } */

extern void abort (void);

int e[100], n, here;

void __attribute__((noinline))
foo(void)
{
  int i, k = 0;
  for (i = 0; i < n; ++i) { e[k] = 10; ++k; e[k] = 10; ++k; }
  for (i = 0; i < k; ++i) here = 1;
  if (here != 1)
    abort ();
}

int main(void)
{
  n = 10;
  foo();
  return 0;
}
