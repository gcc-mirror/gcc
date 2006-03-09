/* { dg-do compile } */
/* { dg-require-effective-target tls } */

extern char buf[];
#pragma omp threadprivate (buf)	/* { dg-error "has incomplete type" } */

void
foo (void)
{
  int i;
#pragma omp threadprivate (i) /* { dg-error "automatic variable" } */
  i = 0;
}
