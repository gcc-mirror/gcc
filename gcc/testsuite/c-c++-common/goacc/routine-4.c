
void gang (void);
void worker (void);
void vector (void);

#pragma acc routine (gang) gang
#pragma acc routine (worker) worker
#pragma acc routine (vector) vector
  
#pragma acc routine seq
void seq (void)
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();  /* { dg-error "routine call uses" } */
  vector ();  /* { dg-error "routine call uses" } */
  seq ();
}

void vector (void) /* { dg-message "declared here" 1 } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();  /* { dg-error "routine call uses" } */
  vector ();
  seq ();
}

void worker (void) /* { dg-message "declared here" 2 } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();
  vector ();
  seq ();
}

void gang (void) /* { dg-message "declared here" 3 } */
{
  gang ();
  worker ();
  vector ();
  seq ();
}
