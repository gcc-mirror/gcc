/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test passing decimal scalars and typedef'd classes by value.  */

extern void pass_5_x (void);
int fails;

int
main ()
{
  pass_5_x ();
  return 0;
}
