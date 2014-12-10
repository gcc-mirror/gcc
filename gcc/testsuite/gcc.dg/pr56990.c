/* PR sanitizer/56990 */
/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-fsanitize=thread" } */

struct S{};

void foo(struct S *p)
{
  *p = (struct S){};
}
