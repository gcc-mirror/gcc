/* { dg-do compile } */
/* { dg-options "-Os -mhwmult=f5series" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Verify the MSP430 cost model is working as expected for the default ISA
   (msp430x) and f5series hwmult, when compiling at -Os.  */

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
**	MOV.B	#7, R14
**	CALL.*	#__mspabi_slll
** ...
**	MOV.B	#100, R14
**	MOV.B	#0, R15
** ...
**	CALL.*	#__mspabi_mpyl_f5hw
** ...
*/
void foo (void)
{
  /* Use the shift library function for this.  */
  res1 = (a << 16) | b;
  /* Likewise.  */
  res2 *= 128;
  /* Use the hardware multiply library function for this.  */
  res3 *= 100;
}
