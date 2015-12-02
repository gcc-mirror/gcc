/* { dg-do compile } */

#define N 1024

int main (int argc, char* argv[])
{
  int x[N];

#pragma acc data copyin (x[0:N])
  {
    int *xp;
#pragma acc host_data use_device (x)
    {
      /* Here 'x' being implicitly firstprivate for the parallel region
	 conflicts with it being declared as use_device in the enclosing
	 host_data region.  */
#pragma acc parallel copyout (xp)
      {
        xp = x; /* { dg-error "variable 'x' declared in enclosing 'host_data' region" } */
      }
    }
  }

  return 0;
}
