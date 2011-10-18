/* { dg-do run } */

extern void abort (void);
void __attribute__((noinline,noclone))
foo (int ** __restrict__ p, int ** __restrict__ q)
{
  **p = **q;
}
int main()
{
  int x = 0, y = 1, *i = &x, *j = &y;
  foo (&i, &j);
  if (x != 1)
    abort ();
  return 0;
}
