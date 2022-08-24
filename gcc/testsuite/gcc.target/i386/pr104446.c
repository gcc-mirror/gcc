/* PR middle-end/104446 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mrtd" } */

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
