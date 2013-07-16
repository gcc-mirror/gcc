/* PR sanitizer/55702 */
/* { dg-do compile { target { x86_64-*-linux* && lp64 } } } */
/* { dg-options "-fsanitize=thread" } */

void
foo ()
{
  __builtin_return (0);
}
