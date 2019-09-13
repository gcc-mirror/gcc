/* Test valid use of clauses with routine.  */

#pragma acc routine gang
void gang (void) /* { dg-warning "partitioned" 3 } */
{
}

#pragma acc routine worker
void worker (void) /* { dg-warning "partitioned" 2 } */
{
}

#pragma acc routine vector
void vector (void) /* { dg-warning "partitioned" 1 } */
{
}

#pragma acc routine seq
void seq (void)
{
}

#pragma acc routine nohost
void nohost (void)
{
}

int main ()
{
#pragma acc kernels num_gangs (32) num_workers (32) vector_length (32) /* { dg-warning "region contains gang partitioned code but is not gang partitioned" } */
  {
    gang ();
    worker ();
    vector ();
    seq ();
    nohost ();
  }

#pragma acc parallel num_gangs (32) num_workers (32) vector_length (32)
  {
    gang ();
    worker ();
    vector ();
    seq ();
    nohost ();
  }

  return 0;
}
