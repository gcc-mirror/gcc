/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w" } */

int
main (void)
{
  unsigned int a = 1;
  a <<= 31;
  a <<= 1;
  return 0;
}
