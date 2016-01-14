/* { dg-options "-Wmisleading-indentation -Wall -fplugin-arg-location_overflow_plugin-value=0x60000001" } */

/* We use location_overflow_plugin.c, which injects the case that location_t
   values have exceeded LINE_MAP_MAX_LOCATION_WITH_COLS, and hence no column
   numbers are available.  */

/* Verify that we're in column-less mode.  */
extern unknown_type test; /* { dg-error "0: unknown type name" } */

/* PR c++/68819: verify that -Wmisleading-indentation is suppressed.  */

int
fn_1 (int flag)
{
  int x = 4, y = 5;
  if (flag) x = 3; y = 2; /* { dg-message "disabled from this point" } */
  return x * y;
}

/* ...and that a "sorry" is only emitted the first time.  */

int
fn_2 (int flag)
{
  int x = 4, y = 5;
  if (flag) x = 3; y = 2; /* { dg-bogus "sorry" } */
  return x * y;
}
