/* { dg-do run } */

unsigned int a = 1;
int b = -1;
int c = 4;
unsigned long long d;

void __attribute__((noipa))
test (void)
{
  for (int i = 0; i < c; i += 2)
    d = a != (int) b ? (unsigned long long) b : (unsigned long long) a;
}

int
main ()
{
  test ();
  if (d != -1ULL)
    __builtin_abort ();
  return 0;
}
