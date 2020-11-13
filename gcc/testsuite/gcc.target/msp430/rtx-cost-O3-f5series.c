/* { dg-do compile } */
/* { dg-options "-O3 -mhwmult=f5series" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Verify the MSP430 cost model is working as expected for the default ISA
   (msp430x) and f5series hwmult, when compiling at -O3.  */

volatile unsigned long a;
volatile unsigned int b;
volatile unsigned long c;
unsigned long res1;
unsigned long res2;
unsigned long res3;

/*
** foo:
** ...
**	MOV.B	#16, R14
**	CALL.*	#__mspabi_slll
** ...
**	MOV.*	\&res2.*
** ...
**	RLA.*RLC.*
** ...
**	MOV.*	\&res3.*
** ...
**	RLA.*RLC.*
** ...
*/
void foo (void)
{
  /* Use the shift library function for this.  */
  res1 = (a << 16) | b;
  /* Emit 7 inline shifts for this.  */
  res2 *= 128;
  /* Perform this multiplication inline, using addition and shifts.  */
  res3 *= 100;
}
