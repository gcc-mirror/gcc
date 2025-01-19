/* PR c/41045 */
/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-fno-pie" { target pie } } */

int v[42], w[42], x;

asm ("# %0" : "+m" (v));		/* { dg-error "'\\\+' in output operand outside of a function" } */
asm ("# %0" : "=&m" (v));		/* { dg-error "'&' in output operand outside of a function" } */
asm ("# %0, %1" : "=%m" (v), "=m" (w));	/* { dg-error "'%' in output operand outside of a function" } */
asm ("# %0, %1" : : "%m" (v), "m" (w));	/* { dg-error "'%' in input operand outside of a function" } */
