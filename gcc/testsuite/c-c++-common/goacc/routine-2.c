/* Test invalid use of the OpenACC 'routine' directive.  */

#pragma acc routine (nothing) gang /* { dg-error "not been declared" } */


#pragma acc routine nohost nohost /* { dg-error "too many 'nohost' clauses" } */
extern void nohost (void);
