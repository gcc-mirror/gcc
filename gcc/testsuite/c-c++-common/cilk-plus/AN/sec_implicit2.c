/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int argc = 2;
  int array[10][10], array2[10];
  __asm volatile ("" : "+r" (argc));
  array[:][:] = __sec_implicit_index(argc) + array[:][:]; /* { dg-error "__sec_implicit_index parameter" } */
  return 0;
}
