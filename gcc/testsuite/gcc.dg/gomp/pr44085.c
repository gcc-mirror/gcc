/* PR middle-end/44085 */
/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-fopenmp" } */

int thr1, thr2;
#pragma omp threadprivate (thr1, thr2)

void
foo (void)
{
#pragma omp task untied	/* { dg-message "note: enclosing task" } */
  {
    thr1++;		/* { dg-error "used in untied task" } */
    thr2 |= 4;		/* { dg-error "used in untied task" } */
  }
}

void
bar (void)
{
#pragma omp task
  {
    thr1++;
    thr2 |= 4;
  }
}
