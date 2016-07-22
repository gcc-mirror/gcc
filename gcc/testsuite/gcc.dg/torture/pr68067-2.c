/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int main()
{
  int a = -1;
  static int b = -2147483647 - 1;
  static int c = 0;
  int t = a - (b + c*-2);
  if (t != 2147483647)
    __builtin_abort();
  return 0;
}

