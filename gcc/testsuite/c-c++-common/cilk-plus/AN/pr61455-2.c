/* PR c++/61455 */
/* { dg-options "-fcilkplus" } */

int a[3] = {2, 3, 4};

int main ()
{
  int c = 10;
  int b = __sec_reduce_add(a[:]);
  if (b+c != 19)
    __builtin_abort();
  return 0;
}
