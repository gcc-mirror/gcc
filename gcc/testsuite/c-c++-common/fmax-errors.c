/* PR c/44782 */
/* { dg-do compile } */
/* { dg-options "-fmax-errors=3 -Wall" } */

void foo (unsigned int i, unsigned int j)
{
  (i) ();			/* { dg-error "" } */
  (j) ();			/* { dg-error "" } */

  i + j; /* { dg-warning "" }  */

  (k) ();			/* { dg-error "" } */
  /* Make sure we see the notes related to the final error we emit.  */
  /* { dg-message "identifier" "" { target c } 12 } */

  /* Warnings after the final error should not appear.  */
  i + j; /* no warning.  */

  (i*j) ();			/* no error here due to -fmax-errors */

} /* { dg-prune-output "compilation terminated" } */
