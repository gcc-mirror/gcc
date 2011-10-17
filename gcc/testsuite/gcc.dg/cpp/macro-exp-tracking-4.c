/*
  { dg-options "-fshow-column -ftrack-macro-expansion=2" }
  { dg-do compile }
 */

#define SQUARE(A) A * A		/* { dg-message "expansion" } */

void
foo()
{
  SQUARE (1 << 0.1);		/* { dg-message "expanded" } */
}

/* { dg-error "13:invalid operands to binary <<" "" { target *-*-* } { 11 } } */
