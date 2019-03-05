/* Test structures passed by value, including to a function with a
   variable-length argument lists.  All struct members are of type
   _Complex short.  */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

extern void struct_by_value_12_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_12_x ();
  exit (0);
}
