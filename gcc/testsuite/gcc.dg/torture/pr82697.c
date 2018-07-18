/* { dg-do run } */

__attribute__((noinline,noclone))
void test(int *pi, long *pl, int f)
{
  *pl = 0;

  *pi = 1;

  if (f)
    *pl = 2;
}

int main()
{
  void *p = __builtin_malloc(sizeof (long));

  test(p, p, 0);

  if (*(int *)p != 1)
    __builtin_abort ();
  return 0;
}
