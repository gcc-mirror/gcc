extern int i;

void
f_acc_parallel (void)
{
#pragma acc parallel
  {
#pragma acc loop
    for (i = 0; i < 2; ++i)
      ;
  }
}


void
f_acc_kernels (void)
{
#pragma acc kernels
  {
#pragma acc loop
    for (i = 0; i < 2; ++i)
      ;
  }
}


void
f_acc_data (void)
{
#pragma acc data
  {
#pragma acc parallel
    ;

#pragma acc parallel
    {
#pragma acc loop
      for (i = 0; i < 2; ++i)
	;
    }

#pragma acc kernels
    ;

#pragma acc kernels
    {
#pragma acc loop
      for (i = 0; i < 2; ++i)
	;
    }

#pragma acc data
    ;

#pragma acc update host(i)

#pragma acc enter data copyin(i)

#pragma acc exit data delete(i)

#pragma acc data
    {
#pragma acc parallel
      ;

#pragma acc parallel
      {
#pragma acc loop
	for (i = 0; i < 2; ++i)
	  ;
      }

#pragma acc kernels
      ;

#pragma acc kernels
      {
#pragma acc loop
	for (i = 0; i < 2; ++i)
	  ;
      }

#pragma acc data
      ;

#pragma acc update host(i)

#pragma acc enter data copyin(i)

#pragma acc exit data delete(i)
    }
  }
}
