/* PR sanitizer/79897 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=enum -O2" } */

enum E
{
  A = 0,
  B = ~0U + 1LL
} x = A;

int
main ()
{
  return x != A;
}
