/* { dg-require-effective-target dfp } */

/* Test passing decimal classes and scalars by value.  */

extern void pass_4_x (void);
int fails;

int
main ()
{
  pass_4_x ();
  return 0;
}
