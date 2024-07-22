/* { dg-do run } */
/* { dg-require-effective-target builtin_eh_return } */

/* Ensure it runs successfully.  */

__attribute__ ((noipa))
int f (int *a, long offset, void *handler)
{
  if (*a == 5)
    return 5;
  __builtin_eh_return (offset, handler);
}

int main ()
{
  int t = 5;
  if (f (&t, 0, 0) != 5)
    __builtin_abort ();
  return 0;
}
