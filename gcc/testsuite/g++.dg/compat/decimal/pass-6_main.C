/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test passing typedef'd decimal classes and scalars by value.  */

extern void pass_6_x (void);
int fails;

int
main ()
{
  pass_6_x ();
  return 0;
}
