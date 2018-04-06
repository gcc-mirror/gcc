/* PR inline-asm/84625 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef int V __attribute__((vector_size (16)));

void
foo (void)
{
  asm volatile ("# %0" : : "X" ((V) { 1, 2, 3, 4 }));	// { dg-error "invalid vector immediate" }
  asm volatile ("# %0" : : "" ((V) { 2, 3, 4, 5 }));	// { dg-error "invalid vector immediate" }
}
