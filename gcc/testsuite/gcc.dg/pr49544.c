/* PR debug/49544 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */
/* { dg-require-effective-target ptr32plus } */

int baz (int, int, void *);

static inline __attribute__ ((always_inline)) long
foo (int x, int y, void *z)
{
  if (y < 0)
    return baz (x, y, z);
  return 0;
}

long
bar (long x, long y, long z)
{
  return foo (x, y, (void *) z);
}
