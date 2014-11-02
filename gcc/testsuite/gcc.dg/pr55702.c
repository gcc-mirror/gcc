/* PR sanitizer/55702 */
/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-fsanitize=thread" } */

void
foo ()
{
  __builtin_return (0);
}
