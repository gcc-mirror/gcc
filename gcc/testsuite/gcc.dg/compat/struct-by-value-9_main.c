/* Test structures passed by value, including to a function with a
   variable-length argument list.  Each struct contains an array
   of small structs, each with three scalar members.  */

extern void struct_by_value_9_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_9_x ();
  exit (0);
}
