/* PR c/44782 */
/* { dg-do compile } */
/* { dg-options "-fmax-errors=3" } */

void foo (unsigned int i, unsigned int j)
{
  (i) ();			/* { dg-error "" } */
  (j) ();			/* { dg-error "" } */
  (i+j) ();			/* { dg-error "" } */
  (i*j) ();			/* no error here due to -fmax-errors */
} /* { dg-prune-output "compilation terminated" } */
