/* PR target/48767 */

void
foo (__builtin_va_list ap)
{
  __builtin_va_arg (ap, void);
}
