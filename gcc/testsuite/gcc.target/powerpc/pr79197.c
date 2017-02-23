/* PR target/79197 */
/* { dg-do compile } */
/* { dg-options "-O0 -mno-popcntd" } */

unsigned a;

void
foo (void)
{
  a = *(double *) (__UINTPTR_TYPE__) 0x400000;
}
