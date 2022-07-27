/* PR middle-end/106122 */
/* { dg-do compile } */
/* { dg-options "-Oz" } */

register volatile int a __asm__("%esp");
void foo (void *);
void bar (void *);

void
baz (void)
{
  foo (__builtin_return_address (0));
  a = 0;
  bar (__builtin_return_address (0));
}
