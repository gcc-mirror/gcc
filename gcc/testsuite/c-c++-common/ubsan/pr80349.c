/* PR sanitizer/80349 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int var;
long a;

long
fn1 ()
{
  return 0 % ((a & 1) == (7UL & 1));
}

long
fn2 ()
{
  return 0 % ((a & 1) == (1 & 7UL));
}

long
fn3 ()
{
  return 0 % ((1 & a) == (7UL & 1));
}

long
fn4 ()
{
  return 0 % ((1 & a) == (1 & 7UL));
}
