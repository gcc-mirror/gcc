/* Test structures passed by value, including to a function with a
   variable-length argument list.  Tested structs end with double.
   This test was written in response to a layout change for such
   structs for powerpc64-linux that breaks compatibility between
   3.3 and 3.4. */

extern void struct_by_value_20_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_20_x ();
  exit (0);
}
