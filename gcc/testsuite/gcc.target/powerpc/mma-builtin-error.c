/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

typedef unsigned char vec_t __attribute__((vector_size(16)));

void
foo (__vector_quad *dst, vec_t vec0, vec_t vec1) /* { dg-error "argument 5 must be a literal between 0 and 15, inclusive" } */
{
  __builtin_mma_pmxvi8ger4 (dst, vec0, vec1, 15, 15, -1);
}
