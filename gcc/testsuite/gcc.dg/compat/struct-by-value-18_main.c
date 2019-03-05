/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are of type
   _Complex long double.  */
/* { dg-skip-if "Program too big" { "avr-*-*" } } */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

extern void struct_by_value_18_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_18_x ();
  exit (0);
}
