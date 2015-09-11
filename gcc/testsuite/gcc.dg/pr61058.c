/* PR rtl-optimization/61058 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-additional-options "-fno-asynchronous-unwind-tables -mtune=atom" { target i?86-*-* x86_64-*-* } } */

void
foo (void)
{
  __builtin_unreachable ();
}
