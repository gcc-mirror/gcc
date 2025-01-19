/* PR target/118511 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (void)
{
  asm volatile ("# %p0" : : "d" (1));	/* { dg-error "invalid 'asm': invalid constant for output modifier 'p'" } */
  asm volatile ("# %q0" : : "d" (1));	/* { dg-error "invalid 'asm': invalid constant for output modifier 'q'" } */
  asm volatile ("# %r0" : : "d" (1));	/* { dg-error "invalid 'asm': invalid constant for output modifier 'r'" } */
}
