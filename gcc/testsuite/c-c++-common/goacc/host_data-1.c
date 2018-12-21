/* Test valid use of host_data directive.  */

int v1[3][3];

void
f (void)
{
#pragma acc host_data use_device(v1)
  ;

#pragma acc host_data use_device(v1) if_present
  ;
}


void bar (float *, float *);

void
foo (float *x, float *y)
{
  int n = 1 << 10;
#pragma acc data create(x[0:n])
  {
    bar (x, y);

    /* This should fail at run time because y is not mapped.  */
#pragma acc host_data use_device(x,y)
    bar (x, y);

    /* y is still not mapped, but this should not fail at run time but
       continue execution with y remaining as the host address.  */
#pragma acc host_data use_device(x,y) if_present
    bar (x, y);

#pragma acc data copyout(y[0:n])
    {
#pragma acc host_data use_device(x,y)
      bar (x, y);

#pragma acc host_data use_device(x,y) if_present
      bar (x, y);

#pragma acc host_data use_device(x,y) if(x != y)
      bar (x, y);

#pragma acc host_data use_device(x,y) if_present if(x != y)
      bar (x, y);
    }
  }
}
