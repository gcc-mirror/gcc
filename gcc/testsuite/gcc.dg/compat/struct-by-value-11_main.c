/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are of type
   _Complex char.  */

extern void struct_by_value_11_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_11_x ();
  exit (0);
}
