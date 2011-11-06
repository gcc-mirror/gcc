/* { dg-do compile } */

double x, y;

void f2(void)
{
  #pragma omp atomic
    y++;
  #pragma omp atomic
    y--;
  #pragma omp atomic
    ++y;
  #pragma omp atomic
    --y;
  #pragma omp atomic
    y += 1;
  #pragma omp atomic
    y -= x;
  #pragma omp atomic
    y *= 3;
  #pragma omp atomic
    y /= 3;
}
