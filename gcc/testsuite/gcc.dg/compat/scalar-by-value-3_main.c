/* Test passing scalars by value.  This test includes _Complex types
   whose real and imaginary parts can be used in variable-length
   argument lists.  */

extern void scalar_by_value_3_x (void);
extern void exit (int);
int fails;

int
main ()
{
  scalar_by_value_3_x ();
  exit (0);
}
