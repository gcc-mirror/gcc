/* { dg-do run } */
/* { dg-options "-O2" } */
extern void abort ();
extern void exit (int);

int x;

foo()
{
  static int count;
  count++;
  if (count > 1)
    abort ();
}

static inline int
frob ()
{
  int a;
  __asm__ ("mov %1, %0\n\t" : "=r" (a) : "m" (x));
  x++;
  return a;
}

int
main ()
{
  int i;
  for (i = 0; i < 10 && frob () == 0; i++)
    foo();
  exit (0);
}
