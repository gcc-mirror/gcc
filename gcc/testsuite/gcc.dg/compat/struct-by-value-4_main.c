/* Test structures passed by value, including to a function with a
   variable-length argument list.  Each struct contains an array
   of small structs:  1, 2, 4, 8, and 12 bytes for LP64.  */ 

extern void struct_by_value_4_x (void);
extern void exit (int);

int
main ()
{
  struct_by_value_4_x ();
  exit (0);
}
