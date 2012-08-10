/* { dg-do run } */

int main (void)
{
  int x = 1;
  while (x)
    x <<= 1;
  return x;
}
