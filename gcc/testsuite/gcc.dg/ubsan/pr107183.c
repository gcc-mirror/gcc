/* PR target/107183 */
/* { dg-do compile } */
/* { dg-options "-O -fsanitize=float-cast-overflow -fcompare-debug" } */

long double a, b, c;

int
foo (void)
{
  unsigned u = b || __builtin_rintl (c);
  return u + (unsigned) a;
}
