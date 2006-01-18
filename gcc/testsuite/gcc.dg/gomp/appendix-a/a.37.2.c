/* { dg-do compile } */

extern int omp_get_thread_num (void);
void work (int i);
void
correct ()
{
  int i;
#pragma omp parallel private(i)
  {
    i = omp_get_thread_num ();
    work (i);
  }
}
