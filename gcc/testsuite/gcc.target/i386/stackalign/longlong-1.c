/* PR target/39137 */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -mpreferred-stack-boundary=2" } */
/* Make sure dynamic stack realignment isn't performed just because there
   are long long variables.  */
/* { dg-final { scan-assembler-not "and\[lq\]?\[^\\n\]*-8,\[^\\n\]*sp" } } */

void fn (void *);

void f1 (void)
{
  unsigned long long a;
  fn (&a);
}
