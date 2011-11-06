/* Test __atomic routines for invalid memory model errors. This only needs
   to be tested on a single size.  */
/* { dg-do compile } */
/* { dg-require-effective-target sync_int_long } */

int i;

main ()
{

  __atomic_exchange_n (&i, 1); /* { dg-error "too few arguments" } */
  __atomic_exchange_n (&i, 1, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST); /* { dg-error "too many arguments" } */
}
