/* PR target/94461 */
/* { dg-do compile } */
/* { dg-options "-mmmx -mno-sse2" } */

typedef int __v2si __attribute__ ((__vector_size__ (8)));

void
foo (__v2si *a, __v2si *b)
{
  __v2si c = *a;
  *b = (__v2si) __builtin_ia32_pmuludq (c, c);	/* { dg-error "needs isa option" } */
}
