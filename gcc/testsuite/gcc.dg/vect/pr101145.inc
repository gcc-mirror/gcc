TYPE __attribute__ ((noinline))
foo_sign (int *__restrict__ a, int *__restrict__ b, TYPE l, TYPE n)
{
  TYPE i;
  for (i = l; n < i; i += C)
    *a++ = *b++ + 1;
  return i;
}

TYPE __attribute__ ((noinline))
bar_sign (int *__restrict__ a, int *__restrict__ b, TYPE l, TYPE n)
{
  TYPE i;
  for (i = l; i < n; i -= C)
    *a++ = *b++ + 1;
  return i;
}

int __attribute__ ((noinline)) neq (int a, int b) { return a != b; }

int a[1000], b[1000];
int fail;

int
main ()
{
  TYPE res;
  TYPE l;
  TYPE n;
  n = N_BASE;
  l = n - C;
  res = foo_sign (a, b, l, n);
  if (res != l)
    fail++;

  l = n;
  res = foo_sign (a, b, l, n);
  if (res != l)
    fail++;

  l = n + C;
  res = foo_sign (a, b, l, n);
  if (neq ((res - MIN) / C, 0))
    fail++;

  n = N_BASE_DOWN;
  l = n - C;
  res = bar_sign (a, b, l, n);
  if (neq ((MAX - res) / C, 0))
    fail++;

  l = n;
  res = bar_sign (a, b, l, n);
  if (res != l)
    fail++;

  l = n + C;
  res = bar_sign (a, b, l, n);
  if (res != l)
    fail++;

  if (fail)
    __builtin_abort ();
  return 0;
}
