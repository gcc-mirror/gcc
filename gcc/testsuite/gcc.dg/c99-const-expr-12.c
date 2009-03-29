/* Test for constant expressions: VLA size constraints.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
f (int m)
{
  /* An array size that is a constant expression, not just an integer
     constant expression, must be checked for being positive, but only
     an integer constant expression makes it not a VLA (which affects
     certain compatibility checks, in particular).  */
  int a1[0]; /* { dg-error "zero" } */
  int a2[-1]; /* { dg-error "negative" } */
  int a3[(int)(double)0.0]; /* { dg-error "zero" } */
  int a4[(int)-1.0]; /* { dg-error "negative" } */
  int a5[(int)+1.0];
  int a6[(int)+2.0];
  void *p = (m ? &a5 : &a6);
  int a7[(int)1.0];
  int a8[(int)2.0];
  void *q = (m ? &a7 : &a8); /* { dg-error "pointer type mismatch in conditional expression" } */
}
