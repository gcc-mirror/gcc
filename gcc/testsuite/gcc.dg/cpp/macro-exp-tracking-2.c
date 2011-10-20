/* 
   { dg-options "-ftrack-macro-expansion=1" }
   { dg-do compile }
*/

#define OPERATE(OPRD1, OPRT, OPRD2) \
 OPRD1 OPRT OPRD2;		/* { dg-message "expansion" } */

#define SHIFTL(A,B) \
  OPERATE (A,<<,B) /* { dg-message "expanded|expansion" } */

#define MULT(A) \
  SHIFTL (A,1)			/* { dg-message "expanded|expansion" } */

void
foo ()
{
  MULT (1.0);			/* { dg-message "expanded" } */
}

/* { dg-error "invalid operands to binary <<" "" { target *-*-* } { 10 } } */
