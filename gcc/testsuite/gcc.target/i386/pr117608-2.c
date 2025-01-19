/* PR target/117608 */
/* { dg-do compile } */
/* { dg-options "-mno-mmx -mno-sse -mmovrs" } */

int i;

void
foo (void)
{
  __builtin_ia32_prefetch (&i, 2, 0, 0);
  __builtin_ia32_prefetch (&i, 2, 1, 0);
  __builtin_ia32_prefetch (&i, 2, 2, 0);
  __builtin_ia32_prefetch (&i, 2, 3, 0);
}
