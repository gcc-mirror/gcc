/* PR target/69247 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-march=zEC12" { target s390*-*-* } } */

void foo (short *);

void
bar (short x, int y)
{
  if (y)
    x = x << 8 | (unsigned short) x >> 8;
  foo (&x);
}
