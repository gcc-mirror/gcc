/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are type
   _Complex int.  */

extern void struct_by_value_l3_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_13_x ();
  exit (0);
}
