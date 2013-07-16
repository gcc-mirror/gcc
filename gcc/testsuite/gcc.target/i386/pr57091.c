/* { dg-do compile } */
/* { dg-options "-O -mcmodel=large"  { target lp64 } } */
void (*bar)();

void foo (void)
{
  bar ();
}
