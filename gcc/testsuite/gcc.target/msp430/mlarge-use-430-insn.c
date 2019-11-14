/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" "-mcpu=430" "-msmall" } { "" } } */
/* { dg-options "-mlarge -O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Test to verify cases where we can use a 430 insn even in the large memory
   model.  */

int foo[2];

/*
** func:  { target msp430_region_lower }
** ...
**	MOV.W	#-4088, &foo
**	MOV.W	#-8531, &40960
**	MOVX.W	#-16657, &106496
** ...
*/
/*
** func:  { target msp430_region_not_lower }
** ...
**	MOVX.W	#-4088, &foo
**	MOV.W	#-8531, &40960
**	MOVX.W	#-16657, &106496
** ...
*/
void
func (void)
{
  foo[0] = 0xF008;
  (*(int *)0xA000) = 0xDEAD;
  (*(int *)0x1A000) = 0xBEEF;
}
