/*
   { dg-options "-ftrack-macro-expansion=1" }
   { dg-do compile }
*/

#define OPERATE(OPRD1, OPRT, OPRD2) \
do \
{ \
  OPRD1 OPRT OPRD2; /* { dg-message "expansion" }*/ 	   \
} while (0)

#define SHIFTL(A,B) \
  OPERATE (A,<<,B) /* { dg-message "expanded|expansion" } */

void
foo ()
{
  SHIFTL (0.1,0.2); /* { dg-message "expanded" } */
}

/* { dg-error "invalid operands" "" { target *-*-* } 13 } */
