/* Test function return values.  Tested structs end with double.  This
   was written in response to a layout change for such structs for
   powerpc64-linux that breaks compatibility between 3.3 and 3.4.  */

extern void struct_return_20_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_return_20_x ();
  exit (0);
}
