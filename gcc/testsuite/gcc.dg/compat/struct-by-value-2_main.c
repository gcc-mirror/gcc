/* Test structures passed by value, including to a function with a
   variable-length argument list.
   This test is based on one contributed by Alan Modra.  */

extern void struct_by_value_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_2_x ();
  exit (0);
}
