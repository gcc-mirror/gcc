/* { dg-do compile } */

extern void bar(void);
void foo(void)
{
  #pragma omp parallel num_threads (0)	/* { dg-warning "must be positive" } */
    {
      bar ();
    }
}
