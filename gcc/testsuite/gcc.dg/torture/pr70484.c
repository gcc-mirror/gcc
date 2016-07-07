/* { dg-do run } */

extern void abort (void);

int __attribute__((noinline,noclone))
f(int *pi, long *pl)
{
  *pi = 1;
  *pl = 0;
  return *(char *)pi;
}

int main()
{
  union { long l; int i; } a;
  if (f (&a.i, &a.l) != 0)
    abort ();
  return 0;
}
