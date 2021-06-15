/* PR target/100777 */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target fopenmp } */
/* { dg-options "-O1 -mdejagnu-cpu=power10 -fopenmp" } */

/* Verify we do not ICE on the following.  */

void
foo (__vector_quad *dst)
{
#pragma omp parallel
  {
    __builtin_mma_xxsetaccz (dst);
  }
}

void
bar (__vector_quad *dst, __vector_quad *src)
{
#pragma omp parallel
  {
    __builtin_mma_disassemble_acc (dst, src);
  }
}
