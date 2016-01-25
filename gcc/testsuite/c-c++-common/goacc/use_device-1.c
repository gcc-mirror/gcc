/* { dg-do compile } */

void bar (float *, float *);

void
foo (float *x, float *y)
{
  int n = 1 << 10;
#pragma acc data create(x[0:n]) copyout(y[0:n])
  {
#pragma acc host_data use_device(x,y)
    bar (x, y);
  }
}
