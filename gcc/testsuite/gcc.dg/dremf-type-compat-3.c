/* Test for bogus diagnostics for dremf definition.  Although this
   definition is formally incorrect in ISO C, a GNU extension permits
   a prototype followed by unpromoted types in a function definition,
   so it should be permitted when the function is built in.  Bug
   16666.  Test with -pedantic-errors, where the problem should still
   be diagnosed.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

float dremf (float, float); /* { dg-error "error: prototype declaration" } */

float
dremf (x, y)
     float x;
     float y;
{ /* { dg-error "error: promoted argument '.' doesn't match prototype" } */
  return x + y;
}
