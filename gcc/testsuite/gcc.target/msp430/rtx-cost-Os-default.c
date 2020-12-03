/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Verify the MSP430 cost model is working as expected for the default ISA
   (msp430x) and hwmult (none), when compiling at -Os.  */

char arr[2];
char a;
char *ptr;

/*
** foo:
** ...
**	MOV.B	\&a, \&arr\+1
**	MOV.*	#arr\+2, \&ptr
** ...
*/

void
foo (void)
{
  arr[1] = a;
  ptr = arr + 2;
}

extern void ext (void);

/*
** bar:
** ...
**	MOV.*	#ext, R10
**	CALL.*	R10
**	CALL.*	R10
** ...
*/

void
bar (void)
{
  ext ();
  ext ();
}
