// { dg-skip-if "not yet" { c++ } }

void foo(int i)
{
  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc parallel // { dg-warning "statement will never be executed" }
    { case 0:; }
  }

  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc kernels // { dg-warning "statement will never be executed" }
    { case 0:; }
  }

  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc serial // { dg-warning "statement will never be executed" }
    { case 0:; }
  }

  switch (i) // { dg-error "invalid entry to OpenACC structured block" }
  {
  #pragma acc data // { dg-warning "statement will never be executed" }
    { case 0:; }
  }
}
