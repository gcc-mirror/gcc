/* Test the ggcplug plugin.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

int main()
{
  int i=0, j=0;
  for (i= 0; i<1000; i++)
    if (i%8 == 0)
      j++;
  return 0;
}
