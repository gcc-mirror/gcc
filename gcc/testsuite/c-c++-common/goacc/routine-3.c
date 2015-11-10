#pragma acc routine gang
void gang (void) /* { dg-message "declared here" 3 } */
{
}

#pragma acc routine worker
void worker (void) /* { dg-message "declared here" 2 } */
{
}

#pragma acc routine vector
void vector (void) /* { dg-message "declared here" 1 } */
{
}

#pragma acc routine seq
void seq (void)
{
}

int main ()
{

#pragma acc parallel num_gangs (32) num_workers (32) vector_length (32)
  {
    #pragma acc loop gang /* { dg-message "loop here" 1 } */
    for (int i = 0; i < 10; i++)
      {
	gang (); /*  { dg-error "routine call uses same" } */
	worker ();
	vector ();
	seq ();
      }
    #pragma acc loop worker /* { dg-message "loop here" 2 } */
    for (int i = 0; i < 10; i++)
      {
	gang (); /*  { dg-error "routine call uses same" } */
	worker (); /*  { dg-error "routine call uses same" } */
	vector ();
	seq ();
      }
    #pragma acc loop vector /* { dg-message "loop here" 3 } */
    for (int i = 0; i < 10; i++)
      {
	gang (); /*  { dg-error "routine call uses same" } */
	worker (); /*  { dg-error "routine call uses same" } */
	vector (); /*  { dg-error "routine call uses same" } */
	seq ();
      }
  }

  return 0;
}
