/* { dg-do run } */

int a = 1, *c = &a, d;
signed char b;

int main ()
{
  for (; b > -27; b--)
    {
      *c ^= b;
      *c ^= 1;
    }
  while (a > 1)
    ;
  return 0; 
}
