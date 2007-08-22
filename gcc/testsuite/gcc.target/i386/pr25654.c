/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mpreferred-stack-boundary=2 -march=i686 -frename-registers" } */

extern void abort (void) __attribute__((noreturn));

struct wrapper {
union setconflict
{
  short a[20];
  int b[10];
} a;
};

int
main ()
{
  int sum = 0;
  {
    struct wrapper a;
    short *c;
    c = a.a.a;
    asm ("": "=r" (c):"0" (c));
    *c = 0;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }
  {
    struct wrapper a;
    int *c;
    c = a.a.b;
    asm ("": "=r" (c):"0" (c));
    *c = 1;
    asm ("": "=r" (c):"0" (c));
    sum += *c;
  }

  if (sum != 1)
    abort();
  return 0;
}

