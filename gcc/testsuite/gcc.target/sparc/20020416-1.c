/* PR bootstrap/6315 */

/* { dg-do compile } */
/* { dg-options "-O2 -mhard-quad-float" } */

void bar (const char *, ...);

void
foo (const char *x, long double y, int z)
{
  if (z >= 0)
    bar (x, z, y);
  else
    bar (x, y);
}
