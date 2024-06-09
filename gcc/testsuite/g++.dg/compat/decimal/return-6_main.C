/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test function return values for typedef'd decimal float classes
   and scalars.  */

extern void return_6_x (void);
int fails;

int
main ()
{
  return_6_x ();
  return 0;
}
