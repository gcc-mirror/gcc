/* { dg-do run } */

extern void abort (void) __attribute__((noreturn));

union setconflict
{
  int a[20];
  long b[10];
};

int
main ()
{
  int sum = 0;
  {
    union setconflict a;
    int *c;
    c = a.a;
    asm ("": "=r" (c):"0" (c));
    *c = 0;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }
  {
    union setconflict a;
    long *c;
    c = a.b;
    asm ("": "=r" (c):"0" (c));
    *c = 1;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }

  if (sum != 1)
    abort();
  return 0;
}
