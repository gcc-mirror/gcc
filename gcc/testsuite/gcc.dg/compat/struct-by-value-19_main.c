/* Test structures passed by value, including to a function with a
   variable-length argument list.  Struct members are char, int, double,
   and other structs containing these types.  This test was written in
   response to a layout change for such structs for powerpc64-linux,
   but this test only checks similar structs that are not affected by
   that break in compatibility. */

extern void struct_by_value_19_x (void);
extern void exit (int);
int fails;

int
main ()
{
  struct_by_value_19_x ();
  exit (0);
}
