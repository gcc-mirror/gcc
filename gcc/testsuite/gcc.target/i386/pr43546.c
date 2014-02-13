/* PR target/43546 */
/* { dg-do compile } */
/* { dg-options "-O1" } */
/* { dg-additional-options "-mpreferred-stack-boundary=2 -msseregparm -msse" { target ia32 } } */

extern void bar (double);

void
foo (void)
{
  bar (1.0);
}
