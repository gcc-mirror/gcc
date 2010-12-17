/* { dg-do compile } */
/* { dg-require-effective-target tls } */

int counter = 0;
#pragma omp threadprivate(counter)
int
increment_counter ()
{
  counter++;
  return (counter);
}
