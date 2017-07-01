/* PR sanitizer/81262 */

void bar (void) __attribute__((cold, noreturn));

int
foo (void)
{
  asm goto ("" : : : : l1, l2);
  bar ();
 l1:
  return 1;
 l2:
  return 0;
}
