/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */
/* { dg-final { check-function-bodies "**" "" } } */

long foo (long a, long b)
__attribute__((target("tune=rocket")));

long foo (long a, long b)
__attribute__((target("tune=sifive-u74")));

/*
** foo:
**   ...
**   # tune = sifive-7-series
**   ...
*/
long foo (long a, long b)
{
  return a + (b * 2);
}
