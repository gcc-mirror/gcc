/* { dg-require-effective-target dfp } */

/* Test function return values for decimal classes.  */

extern void return_2_x (void);
int fails;

int
main ()
{
  return_2_x ();
  return 0;
}
