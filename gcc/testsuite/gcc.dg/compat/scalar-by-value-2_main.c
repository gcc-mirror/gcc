/* Test passing scalars by value.  This test includes scalar types that
   are not supported by va_arg; since they require casts to pass to a
   function with a variable argument list, testing them with variable
   arguments is not interesting.  */

extern void scalar_by_value_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  scalar_by_value_2_x ();
  exit (0);
}
