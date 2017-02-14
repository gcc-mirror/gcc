/* { dg-require-effective-target int32plus } */
/* { dg-do run } */

int main ()
{
  int a = 2;
  int b = 1;

  int t = -1 * ( -0x40000000 * a / ( -0x20000000 + b ) )  / -1;

  if (t != 4) __builtin_abort();

  return 0;
}
