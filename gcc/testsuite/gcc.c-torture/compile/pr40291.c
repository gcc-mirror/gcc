/* PR middle-end/40291 */

int
foo (void *x, char *y, unsigned long long z)
{
  return memcmp (x, y, z);
}
