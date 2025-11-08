/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

#define vect16 __attribute__((vector_size(16)))
void ub_set() {
  volatile vect16 unsigned BS_VAR_0;
  BS_VAR_0[12] = 4;
}
