/* Test function return values.  For this test, all struct members are
   scalar floating point types.  */

extern void struct_return_10_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_return_10_x ();
  exit (0);
}
