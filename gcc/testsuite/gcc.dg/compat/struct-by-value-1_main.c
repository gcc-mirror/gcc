/* Test structure passing by value, using a test from gcc.dg.
   Each struct that is passed contains an array of unsigned char.  */

extern void struct_by_value_1_x (void);
extern void exit (int);

int
main ()
{
  struct_by_value_1_x ();
  exit (0);
}
