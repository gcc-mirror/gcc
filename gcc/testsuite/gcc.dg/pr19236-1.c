/* PR target/19236 */
/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-ffast-math" } */

extern float log1pf (float);
extern double log1p (double);  

float testf (float __x) { 
  return log1pf(1.0); 
}

double test (double __x) { 
  return log1p(1.0); 
}
