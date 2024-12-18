/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fno-pie" { target pie } } */

extern int v[42];

asm ("# %0" : "=:" (32));		/* { dg-error "lvalue required in 'asm' statement" } */
					/* { dg-error "':' constraint used for output operand" "" { target *-*-* } .-1 } */
asm ("# %0" : "=:" (v));		/* { dg-error "':' constraint used for output operand" } */
asm ("# %0" : : "i:" (v));		/* { dg-error "':' constraint mixed with other constraints" } */
asm ("# %0" : : ":i" (v));		/* { dg-error "':' constraint mixed with other constraints" } */
asm ("# %0" : : ",:" (v));		/* { dg-error "':' constraint mixed with other constraints" } */
asm ("# %0" : : ":,:" (v));
asm ("# %0" : : ":," (v));		/* { dg-error "':' constraint mixed with other constraints" } */
asm ("# %0" : : ":,,:" (v));		/* { dg-error "':' constraint mixed with other constraints" } */
asm ("" : : ":" (0));			/* { dg-error "constraint operand is not address of a function or non-automatic variable" } */

void
foo (int x)
{
  int y;
  l:;
  asm ("" : : ":" (&x));		/* { dg-error "constraint operand is not address of a function or non-automatic variable" } */
  asm ("" : : ":" (&&l));		/* { dg-error "constraint operand is not address of a function or non-automatic variable" } */
  asm ("" : : ":" (&y));		/* { dg-error "constraint operand is not address of a function or non-automatic variable" } */
  asm ("" : : ":" (0));			/* { dg-error "constraint operand is not address of a function or non-automatic variable" } */
}
