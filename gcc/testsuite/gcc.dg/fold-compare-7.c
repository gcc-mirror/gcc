/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef float vecf __attribute__((vector_size(8*sizeof(float))));

long f(vecf *f1, vecf *f2){
  return ((*f1 == *f2) < 0)[2];
}
