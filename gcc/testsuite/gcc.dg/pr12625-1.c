/* Warning for assignment used as truth-value should apply for
   bit-fields.  */
/* Origin: bug 12625 from nomura at netapp.com */
/* { dg-do compile } */
/* { dg-options "-Wparentheses" } */

static struct { int i:8; } s; 

void
foo ()
{
  if (s.i = 0) ; /* { dg-warning "parentheses" "warning for bit-field" } */
}
