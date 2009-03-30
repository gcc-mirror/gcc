/* Excess precision tests.  Test diagnostics for excess precision of
   constants.  */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-fexcess-precision=standard" } */

float f = 0.0f * 1e50f; /* { dg-warning "floating constant exceeds range of 'float'" } */
double d = 0.0 * 1e400; /* { dg-warning "floating constant exceeds range of 'double'" } */
