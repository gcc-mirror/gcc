/* 
   { dg-options "-ftrack-macro-expansion=1" }
   { dg-do compile }
*/

#define OPERATE(OPRD1, OPRT, OPRD2) \
 OPRD1 OPRT OPRD2;	  /* { dg-message "in definition of macro 'OPERATE'" } */

#define SHIFTL(A,B) \
  OPERATE (A,<<,B) /* { dg-message "invalid operands to binary <<" } */

#define MULT(A) \
  SHIFTL (A,1)	   /* { dg-message "in expansion of macro 'SHIFTL'" } */

void
foo ()
{
  MULT (1.0);	   /* { dg-message "in expansion of macro 'MULT'" } */
}

