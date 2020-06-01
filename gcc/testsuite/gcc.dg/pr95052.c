/* PR middle-end/95052 */
/* { dg-do compile } */
/* { dg-options "-O2 -fconserve-stack" } */

void bar (char *);

void
foo (void)
{
  char buf[70] = "";
  bar (buf);
}
