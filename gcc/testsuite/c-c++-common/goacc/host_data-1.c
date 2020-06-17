/* Test valid use of host_data directive.  */

/* { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" } */

int v1[3][3];

void
f (void)
{
#pragma acc host_data use_device(v1)
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(v1\\)$" 1 "original" } }
     { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(v1\\)$" 1 "gimple" } } */
  ;

#pragma acc host_data use_device(v1) if_present
  /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data if_present use_device_ptr\\(v1\\)$" 1 "original" } }
     { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data if_present use_device_ptr\\(if_present:v1\\)$" 1 "gimple" } } */
  ;
}


void bar (float *, float *);

void
foo (float *x, float *y, float *yy)
{
  int n = 1 << 10;
#pragma acc data create(x[0:n])
  {
    bar (x, y);

    /* This should fail at run time because y is not mapped.  */
#pragma acc host_data use_device(x,y)
    /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(y\\) use_device_ptr\\(x\\)$" 1 "original" } }
       { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(y\\) use_device_ptr\\(x\\)$" 1 "gimple" } } */
    bar (x, y);

    /* y is still not mapped, but this should not fail at run time but
       continue execution with y remaining as the host address.  */
#pragma acc host_data use_device(x,y) if_present
    /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data if_present use_device_ptr\\(y\\) use_device_ptr\\(x\\)$" 1 "original" } }
       { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data if_present use_device_ptr\\(if_present:y\\) use_device_ptr\\(if_present:x\\)$" 1 "gimple" } } */
    bar (x, y);

#pragma acc data copyout(yy[0:n])
    {
#pragma acc host_data use_device(x,yy)
      /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "original" } }
	 { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "gimple" } } */
      bar (x, yy);

#pragma acc host_data use_device(x,yy) if_present
      /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data if_present use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "original" } }
	 { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data if_present use_device_ptr\\(if_present:yy\\) use_device_ptr\\(if_present:x\\)$" 1 "gimple" } } */
      bar (x, yy);

#pragma acc host_data use_device(x,yy) if(x != yy)
      /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data if\\(x \\!= yy\\) use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "original" } }
	 { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data if\\(D\\.\[0-9\]+\\) use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "gimple" } } */
      bar (x, yy);

#pragma acc host_data use_device(x,yy) if_present if(x == yy)
      /* { dg-final { scan-tree-dump-times "(?n)#pragma acc host_data if\\(x == yy\\) if_present use_device_ptr\\(yy\\) use_device_ptr\\(x\\)$" 1 "original" } }
	 { dg-final { scan-tree-dump-times "(?n)#pragma omp target oacc_host_data if\\(D\\.\[0-9\]+\\) if_present use_device_ptr\\(if_present:yy\\) use_device_ptr\\(if_present:x\\)$" 1 "gimple" } } */
      bar (x, yy);
    }
  }
}
