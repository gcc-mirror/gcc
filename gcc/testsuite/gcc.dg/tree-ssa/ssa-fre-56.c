/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int x = 1;
int __attribute__((noinline,noclone)) fn ()
{
  return x;
}
int (*f)();
int main ()
{
  int res;
  f = fn;
  x = 0;
  res = f ();
  res += x;
  if (res != 0)
    __builtin_abort ();
  return 0;
}

/* We should be able to optimize the load from x in main and thus the
   addition.  */

/* { dg-final { scan-tree-dump-times "= x;" 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times " \\\+ " 0 "fre1" } } */
