/* PR middle-end/36013 */

extern void abort (void);

void __attribute__((noinline))
foo (int **__restrict p, int **__restrict q)
{
  *p[0] = 1;
  *q[0] = 2;
  if (*p[0] != 2)
    abort ();
}

int
main (void)
{
  int a;
  int *p1 = &a, *p2 = &a;
  foo (&p1, &p2);
  return 0;
}
