/* PR tree-optimization/113464 */
/* { dg-do compile { target bitint65535 } } */
/* { dg-options "-O2 -w -std=gnu23" } */

_BitInt(65532) i;

void
foo (void)
{
  __asm__ ("" : "+r" (i));	/* { dg-error "impossible constraint" } */
}
