/* PR debug/46255 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug -fprofile-generate -O" } */

int bar (void);

void
foo (int i)
{
  while (i)
    i = bar ();
}
