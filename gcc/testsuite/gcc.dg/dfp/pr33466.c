/* { dg-do compile } */

/* The suffix for a decimal float constant must use a single case.

   These are invalid for all targets, not just those that support
    decimal float.  */

long double dF = 4.5dF;		/* { dg-error "invalid suffix" } */
long double Df = 4.5Df;		/* { dg-error "invalid suffix" } */
long double dD = 4.5dD;		/* { dg-error "invalid suffix" } */
long double Dd = 4.5Dd;		/* { dg-error "invalid suffix" } */
long double dL = 4.5dL;		/* { dg-error "invalid suffix" } */
long double Dl = 4.5Dl;		/* { dg-error "invalid suffix" } */
