/* { dg-require-effective-target dfp } */

/* Test function return values for decimal float scalars and classes.  */

extern void return_3_x (void);
int fails;

int
main ()
{
  return_3_x ();
  return 0;
}
