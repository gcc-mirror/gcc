// { dg-skip-if "not yet" { c++ } }

void foo()
{
  int l;

  bad1:
  #pragma acc parallel
    goto bad1; // { dg-error "invalid branch to/from OpenACC structured block" }
  #pragma acc kernels
    goto bad1; // { dg-error "invalid branch to/from OpenACC structured block" }
  #pragma acc serial
    goto bad1; // { dg-error "invalid branch to/from OpenACC structured block" }
  #pragma acc data
    goto bad1; // { dg-error "invalid branch to/from OpenACC structured block" }
  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (l = 0; l < 2; ++l)
      goto bad1; // { dg-error "invalid branch to/from OpenACC structured block" }

  goto bad2_parallel; // { dg-error "invalid entry to OpenACC structured block" }
  #pragma acc parallel
    {
      bad2_parallel: ;
    }

  goto bad2_kernels; // { dg-error "invalid entry to OpenACC structured block" }
  #pragma acc kernels
    {
      bad2_kernels: ;
    }

  goto bad2_serial; // { dg-error "invalid entry to OpenACC structured block" }
  #pragma acc serial
    {
      bad2_serial: ;
    }

  goto bad2_data; // { dg-error "invalid entry to OpenACC structured block" }
  #pragma acc data
    {
      bad2_data: ;
    }

  goto bad2_loop; // { dg-error "invalid entry to OpenACC structured block" }
  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
  for (l = 0; l < 2; ++l)
    {
      bad2_loop: ;
    }

  #pragma acc parallel
    {
      int i;
      goto ok1_parallel;
      for (i = 0; i < 10; ++i)
	{ ok1_parallel: break; }
    }

  #pragma acc kernels
    {
      int i;
      goto ok1_kernels;
      for (i = 0; i < 10; ++i)
	{ ok1_kernels: break; }
    }

  #pragma acc serial
    {
      int i;
      goto ok1_serial;
      for (i = 0; i < 10; ++i)
	{ ok1_serial: break; }
    }

  #pragma acc data
    {
      int i;
      goto ok1_data;
      for (i = 0; i < 10; ++i)
	{ ok1_data: break; }
    }

  #pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (l = 0; l < 2; ++l)
      {
	int i;
	goto ok1_loop;
	for (i = 0; i < 10; ++i)
	  { ok1_loop: break; }
      }
}
