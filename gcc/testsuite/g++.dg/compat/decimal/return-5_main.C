/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test function return values for decimal float scalars and typedef'd
   classes.  */

extern void return_5_x (void);
int fails;

int
main ()
{
  return_5_x ();
  return 0;
}
