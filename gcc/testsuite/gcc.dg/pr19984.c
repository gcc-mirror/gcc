/* PR c/19984 */
/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */


double nan (const char *);

const double nok = nan ("");	/* { dg-warning "(not constant)|(near initialization)" } */

const double ok = __builtin_nan ("");

double
foo ()
{
  double ok2 = nan ("");
  return ok2;
}
