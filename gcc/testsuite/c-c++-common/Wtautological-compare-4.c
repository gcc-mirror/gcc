/* PR c++/67863 */
/* { dg-do compile } */
/* { dg-options "-Wtautological-compare" } */

extern int e;
#define A (e ? 4 : 8)
#define B (e ? 4 : 8)

int
fn (void)
{
  if (A <= B)
    return 1;
  return 0;
}
