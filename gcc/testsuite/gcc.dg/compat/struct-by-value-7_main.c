/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are long double
   scalars.  */

extern void struct_by_value_7_x (void);
extern void exit (int);

int
main ()
{
  struct_by_value_7_x ();
  exit (0);
}
