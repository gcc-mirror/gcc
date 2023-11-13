/* { dg-require-effective-target label_values } */

int
f (void)
{
  void *x = &&L2;
  if (&&L3 - &&L1 > 1)
    __builtin_abort();
 L1: return 1;
 L2: __builtin_abort ();
 L3:;
}
