/* PR middle-end/40291 */
/* { dg-additional-options "-fpermissive" } */

int
foo (void *x, char *y, unsigned long long z)
{
  return memcmp (x, y, z);
}
