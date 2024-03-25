/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double x[1024];

void foo (double *p)
{
  x[0] = 1.;
  x[1] = 2.;
  *p = 7.; // aliasing store
  x[0] = x[0] + 1;
  x[1] = x[1] + 1;
  *p = 8.; // aliasing store
  x[1] = x[1] + 1;
  x[0] = x[0] + 1;
}

/* See that we vectorize three SLP instances.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 3 "slp2" { target { ! { s390*-*-* riscv*-*-* } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 5 "slp2" { target {   s390*-*-* riscv*-*-* } } } } */
