/* Test invalid use of the OpenACC 'routine' directive.  */

#pragma acc routine (nothing) gang /* { dg-error "not been declared" } */
