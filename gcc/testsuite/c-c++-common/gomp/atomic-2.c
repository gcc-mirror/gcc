/* { dg-do compile } */

float x, y;

void f1(void)
{
  #pragma omp atomic
    x++;
  #pragma omp atomic
    x--;
  #pragma omp atomic
    ++x;
  #pragma omp atomic
    --x;
  #pragma omp atomic
    x += 1;
  #pragma omp atomic
    x -= y;
  #pragma omp atomic
    x *= 3;
  #pragma omp atomic
    x /= 3;
}
