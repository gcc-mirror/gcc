/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */

int counter = 0;
#pragma omp threadprivate(counter)
int
increment_counter ()
{
  counter++;
  return (counter);
}
