/* { dg-do compile } */
/* { dg-options "-Wall -O2" } */

unsigned char buffer[8];
unsigned long
foo (void)
{
  unsigned long i;
  i = buffer[0];
  if (i >= 8)
    return i - 7;
  i++;
  while (i > 8)
    {
      if (buffer[i-1] != 0)
        return 0;
      i--;
    }
  return 1;
}
