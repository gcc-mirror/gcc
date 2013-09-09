/* Since z196 the nearest integer functions can be expanded to single
   instructions.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z196 -mzarch" } */

extern float ceilf (float x);
extern double ceil (double x);
extern long double ceill (long double x);
extern float floorf (float x);
extern double floor (double x);
extern long double floorl (long double x);
extern float truncf (float x);
extern double trunc (double x);
extern long double truncl (long double x);
extern float nearbyintf (float x);
extern double nearbyint (double x);
extern long double nearbyintl (long double x);
extern float rintf (float x);
extern double rint (double x);
extern long double rintl (long double x);

float my_ceilf (float x) { return ceilf (x); }
double my_ceil (double x) { return ceil (x); }
long double my_ceill (long double x) { return ceill (x); }

float my_floorf (float x) { return floorf (x); }
double my_floor (double x) { return floor (x); }
long double my_floorl (long double x) { return floorl (x); }

float my_truncf (float x) { return truncf (x); }
double my_trunc (double x) { return trunc (x); }
long double my_truncl (long double x) { return truncl (x); }

float my_nearbyintf (float x) { return nearbyintf (x); }
double my_nearbyint (double x) { return nearbyint (x); }
long double my_nearbyintl (long double x) { return nearbyintl (x); }

float my_rintf (float x) { return rintf (x); }
double my_rint (double x) { return rint (x); }
long double my_rintl (long double x) { return rintl (x); }

/* { dg-final { scan-assembler-times "fiebr\t" 1 } } */
/* { dg-final { scan-assembler-times "fidbr\t" 1 } } */
/* { dg-final { scan-assembler-times "fixbr\t" 1 } } */
/* { dg-final { scan-assembler-times "fiebra\t" 4 } } */
/* { dg-final { scan-assembler-times "fidbra\t" 4 } } */
/* { dg-final { scan-assembler-times "fixbra\t" 4 } } */
