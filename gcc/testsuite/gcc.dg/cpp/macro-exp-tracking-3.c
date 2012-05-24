/*
  { dg-options "-fshow-column -ftrack-macro-expansion=1" }
  { dg-do compile }
 */

#define SQUARE(A) A * A	 /* { dg-message "in definition of macro 'SQUARE'" } */

void
foo()
{
  SQUARE (1 << 0.1); /* { dg-error "16:invalid operands to binary <<" } */
}
