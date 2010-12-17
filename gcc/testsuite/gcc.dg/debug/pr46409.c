/* PR debug/46409 */
/* { dg-options "-g" } */

int
foo (int (*x) (unsigned long long), unsigned long long y)
{
  unsigned int z = x (y);
  return 0;
}
