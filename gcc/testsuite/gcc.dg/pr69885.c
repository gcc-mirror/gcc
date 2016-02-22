/* PR target/69885 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-m68000" { target m68k*-*-* } } */

void bar (void);

void
foo (long long x)
{
  if (x >> 1)
    bar ();
}
