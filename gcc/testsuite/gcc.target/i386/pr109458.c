/* PR target/109458 */
/* { dg-do compile } */
/* { dg-options "-msse2" } */

void
foo (_Float16 x)
{
  asm volatile ("# %z0" : : "i" (42));	/* { dg-error "invalid 'asm': invalid operand type used with operand code 'z'" } */
  asm volatile ("# %Z0" : : "i" (42));	/* { dg-error "invalid 'asm': invalid operand type used with operand code 'Z'" } */
  asm volatile ("# %z0" : : "x" (x));	/* { dg-error "invalid 'asm': invalid operand size for operand code 'z'" } */
					/* { dg-warning "non-integer operand used with operand code 'z'" "" { target *-*-* } .-1 } */
  asm volatile ("# %Z0" : : "x" (x));	/* { dg-error "invalid 'asm': invalid operand size for operand code 'Z'" } */
}
