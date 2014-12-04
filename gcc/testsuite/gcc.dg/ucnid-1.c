/* { dg-do run } */
/* { dg-options "-std=c99 -g" } */
void abort (void);

int main (void)
{
  int \u00C0 = 1;
  int \u00C1 = 2;
  int \U000000C2 = 3;
  int wh\u00ff = 4;
  int a\u00c4b\u0441\U000003b4e = 5;
  
  if (\u00C0 != 1)
    abort ();
  if (\u00c1 != 2)
    abort ();
  if (\u00C2 != 3)
    abort ();
  if (wh\u00ff != 4)
    abort ();
  if (a\u00c4b\u0441\U000003b4e != 5)
    abort ();
  
  return 0;
}
