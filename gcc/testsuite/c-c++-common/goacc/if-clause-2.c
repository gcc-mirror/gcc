void
f (short c)
{
#pragma acc parallel if(c)
  ;
#pragma acc kernels if(c)
  ;
#pragma acc data if(c)
  ;
#pragma acc update device(c) if(c)
}
