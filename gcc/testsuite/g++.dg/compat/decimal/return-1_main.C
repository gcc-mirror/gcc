/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test function return values for decimal float scalars.  */

extern void return_1_x (void);
int fails;

int
main ()
{
  return_1_x ();
  return 0;
}
