/* Test generic  __atomic routines for various errors.  */
/* { dg-do compile } */
/* { dg-require-effective-target sync_int_long } */
/* { dg-options "-ansi" } */

void f1 (void* p)
{
  __atomic_compare_exchange(p, p, p, 0, 0, 0); /* { dg-error "must be a non-void pointer type" } */
}

void f2 (int n) 
{ 
  int a[n], b[n]; 
  __atomic_load (&a, &b, __ATOMIC_SEQ_CST); /* { dg-error "must be a pointer to a constant size" } */
}

struct s { };
void f3 (void)
{
  struct s a,b;
  __atomic_load (&a, &b, __ATOMIC_SEQ_CST);  /* { dg-error "must be a pointer to a nonzero size" } */
}

void f4 (int a, int b, int c)
{
  __atomic_load (&a, &b, &c,  __ATOMIC_SEQ_CST); /* { dg-error "incorrect number of arguments" } */
}

void f5 (int a, int b)
{
  __atomic_load (&a, b, __ATOMIC_SEQ_CST); /* { dg-error "must be a pointer type" } */
}

void f6 (int a, char b)
{
  __atomic_load (&a, &b, __ATOMIC_SEQ_CST); /* { dg-error "size mismatch in argument" } */
}

void f7 (int a, int b)
{
  __atomic_load (&a, &b, 45); /* { dg-warning "invalid memory model argument" } */
}

void f8 (int a, int b, float c)
{
  __atomic_load (&a, &b, c); /* { dg-error "non-integer memory model argument" } */
}
