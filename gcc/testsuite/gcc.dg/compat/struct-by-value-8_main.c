/* Test structures passed by value, including to a function with a
   variable-length argument list.  Each struct contains an array
   of small structs with two scalar members.  */

extern void struct_by_value_8_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_8_x ();
  exit (0);
}
