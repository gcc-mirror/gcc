/* Test for bogus diagnostics for dremf definition.  Although this
   definition is formally incorrect in ISO C, a GNU extension permits
   a prototype followed by unpromoted types in a function definition,
   so it should be permitted when the function is built in.  Bug
   16666.  */
/* { dg-do compile } */
/* { dg-options "" } */

float dremf (float, float);

float
dremf (x, y)
     float x, y;
{
  return x + y;
}
