short *f(short *a, int b, int *d) __attribute__((noinline,noclone));

short *f(short *a, int b, int *d)
{
  short c = *a;
  a++;
  c = b << c;
  *d = c;
  return a;
}

int main(void)
{
  int d;
  short a[2];
  a[0] = 0;
  if (f(a, 1, &d) != &a[1])
    __builtin_abort ();
  if (d != 1)
    __builtin_abort ();
  return 0;
}
