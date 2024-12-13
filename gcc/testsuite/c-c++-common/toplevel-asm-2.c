/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fno-pie" { target pie } } */

int v[42], w[42], x;
void l1 (void);

asm ("# %0" : "=m" (32));		/* { dg-error "lvalue required in 'asm' statement" } */
asm ("# %0" : "=m" (v) : "0" (v));	/* { dg-warning "matching constraint does not allow a register" } */
asm ("# %0" : : "m" (v), "0" (v));	/* { dg-error "matching constraint references invalid operand number" } */
asm ("# %0" :: "i" (0) : "cc");		/* { dg-error "expected '\\\)' before ':' token" } */
asm ("# %0" : : "i" (0) :: l1);		/* { dg-error "expected '\\\)' before '::?' token" } */
asm ("# %0" : "=r" (x));		/* { dg-error "constraint allows registers outside of a function" } */
asm ("# %0" : "=m" (x++));		/* { dg-error "lvalue required in 'asm' statement" } */
asm ("# %0" : "=m" (v[x]));		/* { dg-error "output operand outside of a function is not constant" } */
asm ("# %0" :: "r" (x));		/* { dg-error "constraint allows registers outside of a function" } */
asm ("# %0" : : "m" (x++));		/* { dg-error "side-effects in input operand outside of a function" } */
asm ("# %0" : : "m" (v[x]));		/* { dg-error "input operand outside of a function is not constant" } */
asm ("# %0" : : "i" (v[x]));		/* { dg-error "input operand outside of a function is not constant" } */
asm ("# %0" : : "i" (x++));		/* { dg-error "side-effects in input operand outside of a function" } */
