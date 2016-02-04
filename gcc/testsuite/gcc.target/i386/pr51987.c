/* PR tree-optimization/51987 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O3" } */

extern void abort (void);
union U { unsigned long long l; struct { unsigned int l, h; } i; };

__attribute__((noinline, noclone)) void
foo (char *x, char *y)
{
  int i;
  for (i = 0; i < 64; i++)
    {
      union U u;
      asm ("movl %1, %k0; salq $32, %0" : "=r" (u.l) : "r" (i));
      x[i] = u.i.h;
      union U v;
      asm ("movl %1, %k0; salq $32, %0" : "=r" (v.l) : "r" (i));
      y[i] = v.i.h;
    }
}

int
main ()
{
  char a[64], b[64];
  int i;
  foo (a, b);
  for (i = 0; i < 64; i++)
    if (a[i] != i || b[i] != i)
      abort ();
  return 0;
}
