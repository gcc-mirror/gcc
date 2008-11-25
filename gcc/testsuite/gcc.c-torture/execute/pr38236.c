struct X { int i; };

int __attribute__((noinline))
foo (struct X *p, int *q, int a, int b)
{
  struct X x, y;
  if (a)
    p = &x;
  if (b)
    q = &x.i;
  else
    q = &y.i;
  *q = 1;
  return p->i; 
}
extern void abort (void);
int main()
{
  if (foo((void *)0, (void *)0, 1, 1) != 1)
    abort ();
  return 0;
}
