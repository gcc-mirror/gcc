float *d_a;

void
f (float *a)
{
#pragma acc parallel copyout (a[3:10]) deviceptr (d_a)
  d_a[2] += 1.0;

#pragma acc parallel deviceptr (d_a) copyout (a[3:10])
  d_a[2] += 1.0;
}
