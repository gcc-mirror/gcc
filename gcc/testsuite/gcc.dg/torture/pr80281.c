/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int
main ()
{
  volatile int a = 0;
  long long b = 2147483648LL;
  int c = a % 2;
  int x = ((int) -b + c) % -2147483647;
  if (x != -1)
    __builtin_abort ();
  return 0;
}
