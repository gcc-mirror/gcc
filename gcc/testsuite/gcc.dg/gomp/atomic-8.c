/* { dg-do compile } */

long double z;

void f3(void)
{
  #pragma omp atomic
    z++;
  #pragma omp atomic
    z--;
  #pragma omp atomic
    ++z;
  #pragma omp atomic
    --z;
  #pragma omp atomic
    z += 1;
  #pragma omp atomic
    z *= 3;
  #pragma omp atomic
    z /= 3;
}
