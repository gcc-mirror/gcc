/* PR target/48767 */
/* { dg-skip-if "invalid use of void expression" { xstormy16-*-* } { "*" } { "" } } */

void
foo (__builtin_va_list ap)
{
  __builtin_va_arg (ap, void);
}
