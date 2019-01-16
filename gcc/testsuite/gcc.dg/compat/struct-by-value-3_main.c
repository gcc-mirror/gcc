/* Test structures passed by value, including to a function with a
   variable-length argument list.  All struct members are scalar
   integral types, and the structs are "small": 1, 2, 4, 8, and 12
   bytes for LP64.  */
/* { dg-skip-if "limited code space" { pdp11-*-* } } */

extern void struct_by_value_3_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_3_x ();
  exit (0);
}
