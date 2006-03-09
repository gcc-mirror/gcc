/* { dg-do compile } */

extern int foo(void);
extern void bar(void);

int main ()
{
  /* Malformed uses of 'if' and 'num_threads'.  */
  #pragma omp parallel if (foo () > 10) if (foo () == 3) /* { dg-error "too many" } */
    {
      bar ();
    }

  #pragma omp parallel num_threads (3) num_threads (20)	/* { dg-error "too many" } */
    {
      bar ();
    }

  /* Valid uses of 'if' and 'num_threads'.  */
  #pragma omp parallel if (foo () == 10) num_threads (foo ())
    {
      bar ();
    }
}
