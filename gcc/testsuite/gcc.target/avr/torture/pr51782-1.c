/* PR middle-end/51782 */
/* { dg-do run } */
/* { dg-options { "-std=gnu99" } } */

#include <stdlib.h>

struct R { char r; };
struct RGB { char r,g,b; };

__flash const struct R r1 = { 12 };
__flash const struct RGB r3 = { 23, 56, 78 };

char __attribute__((noinline,noclone))
read1_bug (const __flash struct R *s)
{
  struct R t = *s;
  return t.r;
}

char __attribute__((noinline,noclone))
read1_ok (const __flash struct R *s)
{
  return s->r;
}

char __attribute__((noinline,noclone))
read3_bug (const __flash struct RGB *s)
{
  struct RGB t = *s;
  return t.r + t.g + t.b;
}

char __attribute__((noinline,noclone))
read3_ok (const __flash struct RGB *s)
{
  return s->r + s->g + s->b;
}

__flash const struct R * volatile p1 = &r1;
__flash const struct RGB * volatile p3 = &r3;

int main (void)
{
  if (read1_bug (p1) != read1_ok (p1))
    abort();
  
  if (read3_bug (p3) != read3_ok (p3))
    abort();
  
  exit (0);
}
