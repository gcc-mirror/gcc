/* Test structures passed by value, including to a function with a
   variable-length argument list.  Each struct contains an array
   of small structs with a single member.  */

extern void struct_by_value_4_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_4_x ();
  exit (0);
}
