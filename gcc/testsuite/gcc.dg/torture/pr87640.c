/* { dg-do compile } */

int main ()
{ 
  unsigned b = 0;
  int c, d = -8;
  for (; b < 2; b++)
    for (c = 1; c; c--)
      d++;
  return 0;
}
