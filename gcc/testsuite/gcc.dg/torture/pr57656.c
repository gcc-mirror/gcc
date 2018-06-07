/* { dg-do run } */
/* { dg-additional-options "-fstrict-overflow" } */

int main (void)
{
  int a = -1;
  int b = __INT_MAX__;
  int c = 2;
  int t = 1 - ((a - b) / c);  // t = 1 - ( __INT_MIN__ / 2 )
  if (t != (1 - (-1 - __INT_MAX__) / 2))
    __builtin_abort();
  return 0;
}
