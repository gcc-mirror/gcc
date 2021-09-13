/* PR target/99025 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse4" } */

long v[16];
int w;
union U { float u; int r; } x;

void
foo (float y)
{
  union U z;
  x.u = w;
  v[5] = x.r;
  z.u = y;
  v[6] = z.r;
}
