/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2 -std=gnu89" } */

/* This compile only test is to detect an assertion failure in stack branch
   development.  */
void baz (void);
                       
double foo (void)
{
  baz ();
  return;
}

double bar (void)
{
  baz ();
}
