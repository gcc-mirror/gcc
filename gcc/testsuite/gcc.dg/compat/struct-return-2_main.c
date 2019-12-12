/* Test function return values.  This test includes structs that are
   arrays of unsigned integral scalars.  */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

extern void struct_return_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_return_2_x ();
  exit (0);
}
