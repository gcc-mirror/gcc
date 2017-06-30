/* PR target/81225 */
/* { dg-do compile } */
/* { dg-options "-mavx512ifma -O3 -ffloat-store" } */

long a[24];
float b[4], c[24];
int d;

void
foo ()
{
  for (d = 0; d < 24; d++)
    c[d] = (float) d ? : b[a[d]];
}
