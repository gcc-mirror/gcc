/* { dg-do compile } */

int thr;
#pragma omp threadprivate (thr)
int foo ()
{
  int l;
#pragma omp parallel copyin (thr) reduction (||:l)
  ;
}
