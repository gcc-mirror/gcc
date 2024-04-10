/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test function return values for decimal float classes and scalars.  */

extern void return_4_x (void);
int fails;

int
main ()
{
  return_4_x ();
  return 0;
}
