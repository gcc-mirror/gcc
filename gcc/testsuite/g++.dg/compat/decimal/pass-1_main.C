/* { dg-require-effective-target dfp } */

/* Test passing decimal scalars by value.  */

extern void pass_1_x (void);
int fails;

int
main ()
{
  pass_1_x ();
  return 0;
}
