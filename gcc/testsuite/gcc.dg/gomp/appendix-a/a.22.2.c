/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */

int
increment_counter_2 ()
{
  static int counter = 0;
#pragma omp threadprivate(counter)
  counter++;
  return (counter);
}
