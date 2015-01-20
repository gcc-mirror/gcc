void
fun1 (void)
{
  char *a = 0;

#pragma acc data deviceptr(a)
  ++a;

#pragma acc data deviceptr(a)
#pragma acc parallel
  ++a;

#pragma acc data deviceptr(a)
#pragma acc parallel deviceptr(a)
  ++a;

#pragma acc data
#pragma acc parallel deviceptr(a)
  ++a;

#pragma acc parallel deviceptr(a)
  ++a;
}
