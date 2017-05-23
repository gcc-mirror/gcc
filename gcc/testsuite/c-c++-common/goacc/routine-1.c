
#pragma acc routine gang
void gang (void)
{
}

#pragma acc routine worker
void worker (void)
{
}

#pragma acc routine vector
void vector (void)
{
}

#pragma acc routine seq
void seq (void)
{
}

int main ()
{
#pragma acc kernels num_gangs (32) num_workers (32) vector_length (32)
  {
    gang ();
    worker ();
    vector ();
    seq ();
  }

#pragma acc parallel num_gangs (32) num_workers (32) vector_length (32)
  {
    gang ();
    worker ();
    vector ();
    seq ();
  }

  return 0;
}
