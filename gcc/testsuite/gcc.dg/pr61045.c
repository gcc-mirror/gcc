/* { dg-do run } */
/* { dg-options "-fstrict-overflow" } */

int main ()
{
  int a = 0;
  int b = __INT_MAX__;
  int t = (a - 2) > (b - 1);
  if (t != 0)
    __builtin_abort();
  return 0;
}
