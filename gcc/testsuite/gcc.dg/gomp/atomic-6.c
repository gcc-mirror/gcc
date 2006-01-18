/* { dg-do compile } */

int x[10], z;
double y[10];

void f1(void)
{
  #pragma omp atomic
    x[z] /= y[z];
}
