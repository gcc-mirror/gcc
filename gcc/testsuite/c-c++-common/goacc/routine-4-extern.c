/* Test invalid intra-routine parallelism.  */
/* Variant of 'routine-4.c', moving the callees 'extern'.  */

extern void extern_gang (void);
#pragma acc routine (extern_gang) gang
extern void extern_worker (void);
#pragma acc routine (extern_worker) worker
extern void extern_vector (void);
#pragma acc routine (extern_vector) vector
extern void extern_seq (void);
#pragma acc routine (extern_seq) seq

void gang (void);
void worker (void);
void vector (void);

#pragma acc routine (gang) gang
#pragma acc routine (worker) worker
#pragma acc routine (vector) vector
  
#pragma acc routine seq
void seq (void)
{
  extern_gang ();  /* { dg-error "routine call uses" } */
  extern_worker ();  /* { dg-error "routine call uses" } */
  extern_vector ();  /* { dg-error "routine call uses" } */
  extern_seq ();

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

void vector (void)
{
  extern_gang ();  /* { dg-error "routine call uses" } */
  extern_worker ();  /* { dg-error "routine call uses" } */
  extern_vector ();
  extern_seq ();

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

void worker (void)
{
  extern_gang ();  /* { dg-error "routine call uses" } */
  extern_worker ();
  extern_vector ();
  extern_seq ();

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

void gang (void)
{
  extern_gang ();
  extern_worker ();
  extern_vector ();
  extern_seq ();

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
