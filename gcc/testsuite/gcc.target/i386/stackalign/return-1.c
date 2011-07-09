/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" } */

/* This compile only test is to detect an assertion failure in stack branch
   development.  */

double
foo (void)
{
}
