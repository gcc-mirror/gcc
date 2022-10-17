/* Test invalid intra-routine parallelism.  */
/* See also variant 'routine-4-extern.c', moving the callees 'extern'.  */

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

#pragma acc loop // { dg-warning "insufficient partitioning" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop gang // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop worker // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop vector // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;
}

void vector (void) /* { dg-message "declared here" "1" } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();  /* { dg-error "routine call uses" } */
  vector ();
  seq ();

#pragma acc loop 
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop gang // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop worker // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop vector
  for (int i = 0; i < 10; i++)
    ;
}

void worker (void) /* { dg-message "declared here" "2" } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();
  vector ();
  seq ();

#pragma acc loop
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop gang // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop worker
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop vector
  for (int i = 0; i < 10; i++)
    ;
}

void gang (void) /* { dg-message "declared here" "3" } */
{
  gang ();
  worker ();
  vector ();
  seq ();

#pragma acc loop
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop gang
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop worker
  for (int i = 0; i < 10; i++)
    ;

#pragma acc loop vector
  for (int i = 0; i < 10; i++)
    ;
}
