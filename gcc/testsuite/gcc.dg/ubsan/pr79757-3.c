/* PR sanitizer/79757 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-fsanitize=undefined" } */

unsigned __int128 x, y;

void
fn1 (z)
  __auto_type z = x / y; /* { dg-error "parameter 'z' is initialized" } */
{
}

void
fn2 (z)
  __auto_type z = x >> y; /* { dg-error "parameter 'z' is initialized" } */
{
}
