/* Test function return values.   This test includes scalar types that
   are not supported by va_arg.  */

extern void scalar_return_2_x (void);
extern void exit (int);
int fails;

int
main ()
{
  scalar_return_2_x ();
  exit (0);
}
