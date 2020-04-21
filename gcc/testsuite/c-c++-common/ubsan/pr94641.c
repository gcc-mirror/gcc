/* PR c/94641 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -Wpadded" } */

void foo (void *) __attribute__((nonnull));

void
bar (void *p)
{
  foo (p);
}
