/* PR target/69015 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-if-conversion" } */

void
foo (int c)
{
  if (c)
    __builtin_trap ();
}
