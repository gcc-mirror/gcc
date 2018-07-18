/* { dg-do compile { target lp64 } } */
/* { dg-options "-O -mcmodel=large" } */
void (*bar)();

void foo (void)
{
  bar ();
}
