/* { dg-do compile } */
/* { dg-options "-fnon-call-exceptions -fsignaling-nans" } */

/* PR tree-optimization/120951 */

/* cdce would create a trapping comparison inside a condition.
   tests to make sure that does not happen.  */

double f(double r, double i) {
   return __builtin_fmod(r, i);
}

