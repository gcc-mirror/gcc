/* Test function return values.  This test includes structs that are
   arrays of unsigned integral scalars.  */

extern void struct_return_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_return_2_x ();
  exit (0);
}
