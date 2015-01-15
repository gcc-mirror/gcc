// { dg-skip-if "not yet" { c++ } }

void foo(int i)
{
  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc parallel
    { case 0:; }
  }

  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc kernels
    { case 0:; }
  }

  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc data
    { case 0:; }
  }
}
