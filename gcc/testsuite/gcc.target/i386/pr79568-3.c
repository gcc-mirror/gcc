/* PR target/79568 */
/* { dg-do compile } */
/* { dg-options "-mno-sahf -mno-mmx -mno-sse" } */
/* { dg-additional-options "-march=i386" { target ia32 } } */

#pragma GCC push_options
#pragma GCC target ("sse")
void
foo (void)
{
  __builtin_ia32_pause ();
}
#pragma GCC pop_options

void
bar (void)
{
  __builtin_ia32_pause ();
}
