/* PR sanitizer/79757 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-Wno-old-style-definition -fsanitize=undefined" } */

unsigned __int128 x, y;

void
fn1 (void)
{
  int a (z)
    unsigned long long z = x / y; /* { dg-error "parameter 'z' is initialized" } */
  {
  }
}

void
fn2 (void)
{
  int a (z)
    unsigned long long z = x >> y; /* { dg-error "parameter 'z' is initialized" } */
  {
  }
}
