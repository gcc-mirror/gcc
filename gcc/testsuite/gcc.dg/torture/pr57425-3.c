/* { dg-do run } */

extern void abort (void) __attribute__((noreturn));

int
main ()
{
  int sum = 0;
  {
    long a[20];
    long *c;
    c = a;
    asm ("": "=r" (c):"0" (c));
    *c = 0;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }
  {
    long long b[10];
    long long *c;
    c = b;
    asm ("": "=r" (c):"0" (c));
    *c = 1;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }

  if (sum != 1)
    abort();
  return 0;
}
