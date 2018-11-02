/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are type
   _Complex int.  */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

extern void struct_by_value_13_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_13_x ();
  exit (0);
}
