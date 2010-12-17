/* PR target/46088 */
/* { dg-do compile } */
/* { dg-options "-Os -fnon-call-exceptions -fpeel-loops" } */

extern void bar (void);

void
foo (int i)
{
  if (i >> 3)
    bar ();
}
