/* Test invalid intra-routine parallelism.  */

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

  int red;

#pragma acc loop reduction (+:red) // { dg-warning "insufficient partitioning" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop gang reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop worker reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop vector reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;
}

void vector (void) /* { dg-message "declared here" 1 } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();  /* { dg-error "routine call uses" } */
  vector ();
  seq ();

  int red;

#pragma acc loop reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop gang reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop worker reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop vector reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;
}

void worker (void) /* { dg-message "declared here" 2 } */
{
  gang ();  /* { dg-error "routine call uses" } */
  worker ();
  vector ();
  seq ();

  int red;

#pragma acc loop reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop gang reduction (+:red) // { dg-error "disallowed by containing routine" }
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop worker reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop vector reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;
}

void gang (void) /* { dg-message "declared here" 3 } */
{
  gang ();
  worker ();
  vector ();
  seq ();

  int red;

#pragma acc loop reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop gang reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop worker reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;

#pragma acc loop vector reduction (+:red)
  for (int i = 0; i < 10; i++)
    red ++;
}
