/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are double
   scalars.  */

extern void struct_by_value_6a_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_6a_x ();
  exit (0);
}
