/* { dg-do compile { target ia64*-hp-hpux* } } */

/* Test that __fpreg is distinct from any other builtin type.  */

extern float fr1; /* { dg-error "" } */
extern __fpreg fr1; /* { dg-error "" } */
extern double fr2; /* { dg-error "" } */
extern __fpreg fr2; /* { dg-error "" } */
extern long double fr3; /* { dg-error "" } */
extern __fpreg fr3; /* { dg-error "" } */
extern __float80 fr4; /* { dg-error "" } */
extern __fpreg fr4; /* { dg-error "" } */
extern __float128 fr5; /* { dg-error "" } */
extern __fpreg fr5; /* { dg-error "" } */

/* Test that __float80 is distinct from any other builtin type.  */

extern float f801; /* { dg-error "" } */
extern __float80 f801; /* { dg-error "" } */
extern double f802; /* { dg-error "" } */
extern __float80 f802; /* { dg-error "" } */
extern long double f803; /* { dg-error "" } */
extern __float80 f803; /* { dg-error "" } */
extern __fpreg f804;  /* { dg-error "" } */
extern __float80 f804; /* { dg-error "" } */
extern __float128 f805; /* { dg-error "" } */
extern __float80 f805; /* { dg-error "" } */

/* Test that __float128 is distinct from any other builtin type --
   except "long double", for which it is a synonym.  */

extern float f1281; /* { dg-error "" } */
extern __float128 f1281; /* { dg-error "" } */
extern double f1282; /* { dg-error "" } */
extern __float128 f1282; /* { dg-error "" } */
extern long double f1283;
extern __float128 f1283;
extern __fpreg f1284; /* { dg-error "" } */
extern __float128 f1284; /* { dg-error "" } */
extern __float80 f1285; /* { dg-error "" } */
extern __float128 f1285; /* { dg-error "" } */
