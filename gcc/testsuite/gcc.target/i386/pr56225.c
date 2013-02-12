/* PR target/56225 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=pentium3 -mtune=generic" } */

void bar (int);

void
foo (int x, int y)
{
  __attribute__ ((vector_size (8 * sizeof (short)))) short s0 = { x };
  bar ((short) (long) &s0 + y);
}
