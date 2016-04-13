/* { dg-options "-fdiagnostics-show-caret -Wmisleading-indentation -Wall -fplugin-arg-location_overflow_plugin-value=0x50000001" } */

/* We use location_overflow_plugin.c, which injects the case that location_t
   values have exceeded LINE_MAP_MAX_LOCATION_WITH_PACKED_RANGES, and hence
   no range-packing should occur.  */

/* Verify that we still have column numbers.  */
extern unknown_type test; /* { dg-error "8: unknown type name" } */

/* ...and ranges.  */
/* { dg-begin-multiline-output "" }
 extern unknown_type test;
        ^~~~~~~~~~~~
   { dg-end-multiline-output "" } */


/* PR c++/68819: verify that -Wmisleading-indentation is still available.  */

int
fn_1 (int flag)
{
  int foo = 4, bar = 5;
  if (flag) foo = 3; bar = 2; /* { dg-warning "this .if." } */
  return foo * bar;
}

/* Verify that we still have ranges, despite the lack of packing.  */

/* { dg-begin-multiline-output "" }
   if (flag) foo = 3; bar = 2;
                      ^~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   if (flag) foo = 3; bar = 2;
   ^~
   { dg-end-multiline-output "" } */
