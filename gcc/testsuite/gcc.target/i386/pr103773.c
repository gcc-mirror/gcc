/* { dg-do run } */
/* { dg-options "-Oz" } */

unsigned long long x;

int main (void)
{
  __builtin_memset (&x, 0xff, 4);
  if (x != 0xffffffff)
    __builtin_abort ();
  return 0;
}
