/* PR target/98585 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mcmodel=large -masm=att" } */
/* { dg-final { scan-assembler "movabs\[^\n\r]*bar" } } */

void bar (void);

void
__attribute__ ((target ("bmi2")))
foo()
{
  bar ();
}
