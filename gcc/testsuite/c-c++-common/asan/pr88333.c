/* PR sanitizer/88333 */
/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-fstack-protector-strong -fsanitize=address" } */

void bar (int *);

void
foo (void)
{
  int c;
  bar (&c);
}
