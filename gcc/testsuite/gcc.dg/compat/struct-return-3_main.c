/* Test function return values.  For this test, all struct members are
   scalar integral types and the structs are "small": 1, 2, 4, 8, and 12
   bytes for LP64.  */

extern void struct_return_3_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_return_3_x ();
  exit (0);
}
