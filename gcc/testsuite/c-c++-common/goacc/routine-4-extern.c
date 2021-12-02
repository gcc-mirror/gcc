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

void vector (void)
{
  extern_gang ();  /* { dg-error "routine call uses" } */
  extern_worker ();  /* { dg-error "routine call uses" } */
  extern_vector ();
  extern_seq ();

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

void worker (void)
{
  extern_gang ();  /* { dg-error "routine call uses" } */
  extern_worker ();
  extern_vector ();
  extern_seq ();

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

void gang (void)
{
  extern_gang ();
  extern_worker ();
  extern_vector ();
  extern_seq ();

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
